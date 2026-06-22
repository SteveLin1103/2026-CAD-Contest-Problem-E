#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <queue>
#include <numeric>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

static constexpr double EPS = 1e-6;
static constexpr double AREA_REL_TOL = 1e-5;

static string trim(const string &s) {
    size_t b = 0, e = s.size();
    while (b < e && isspace(static_cast<unsigned char>(s[b]))) b++;
    while (e > b && isspace(static_cast<unsigned char>(s[e - 1]))) e--;
    string out = s.substr(b, e - b);
    if (out.size() >= 3 &&
        static_cast<unsigned char>(out[0]) == 0xEF &&
        static_cast<unsigned char>(out[1]) == 0xBB &&
        static_cast<unsigned char>(out[2]) == 0xBF) {
        out = out.substr(3);
    }
    return out;
}

static string upperNoSpace(string s) {
    string out;
    for (unsigned char c : s) {
        if (!isspace(c)) out.push_back(static_cast<char>(toupper(c)));
    }
    return out;
}

static string normalizeSpaces(string s) {
    string out;
    bool prev_space = false;
    for (unsigned char c : s) {
        if (isspace(c)) {
            if (!prev_space) out.push_back(' ');
            prev_space = true;
        } else {
            out.push_back(static_cast<char>(c));
            prev_space = false;
        }
    }
    return trim(out);
}

static bool isEmptyRecord(const vector<string> &r) {
    for (const string &v : r) if (!trim(v).empty()) return false;
    return true;
}

static string cell(const vector<string> &r, size_t i) {
    if (i >= r.size()) return "";
    return trim(r[i]);
}

static bool quoteBalanced(const string &record) {
    bool in_quotes = false;
    for (size_t i = 0; i < record.size(); ++i) {
        if (record[i] == '"') {
            if (in_quotes && i + 1 < record.size() && record[i + 1] == '"') ++i;
            else in_quotes = !in_quotes;
        }
    }
    return !in_quotes;
}

static vector<string> parseCsvRecord(const string &record) {
    vector<string> fields;
    string cur;
    bool in_quotes = false;
    for (size_t i = 0; i < record.size(); ++i) {
        char c = record[i];
        if (in_quotes) {
            if (c == '"') {
                if (i + 1 < record.size() && record[i + 1] == '"') {
                    cur.push_back('"');
                    ++i;
                } else {
                    in_quotes = false;
                }
            } else if (c == '\r') {
                // ignore CR
            } else if (c == '\n') {
                cur.push_back(' ');
            } else {
                cur.push_back(c);
            }
        } else {
            if (c == '"') in_quotes = true;
            else if (c == ',') {
                fields.push_back(normalizeSpaces(cur));
                cur.clear();
            } else if (c == '\r') {
                // ignore CR
            } else {
                cur.push_back(c);
            }
        }
    }
    fields.push_back(normalizeSpaces(cur));
    return fields;
}

static vector<vector<string>> readCsvRecords(const string &filename) {
    ifstream fin(filename, ios::binary);
    if (!fin) throw runtime_error("Cannot open input file: " + filename);
    vector<vector<string>> records;
    string line, pending;
    while (getline(fin, line)) {
        if (!pending.empty()) pending.push_back('\n');
        pending += line;
        if (quoteBalanced(pending)) {
            records.push_back(parseCsvRecord(pending));
            pending.clear();
        }
    }
    if (!pending.empty()) throw runtime_error("Malformed CSV: reached EOF inside a quoted field.");
    return records;
}

static double parseDoubleStrict(const string &raw, const string &what, vector<string> &errors) {
    string s = trim(raw);
    if (s.empty()) {
        errors.push_back("Missing numeric value for " + what);
        return 0.0;
    }
    try {
        size_t idx = 0;
        double v = stod(s, &idx);
        string rest = trim(s.substr(idx));
        if (!rest.empty()) errors.push_back("Unexpected trailing text in " + what + ": '" + raw + "'");
        return v;
    } catch (const exception &) {
        errors.push_back("Invalid numeric value for " + what + ": '" + raw + "'");
        return 0.0;
    }
}

static int parseIntStrict(const string &raw, const string &what, vector<string> &errors) {
    string s = trim(raw);
    if (s.empty()) {
        errors.push_back("Missing integer value for " + what);
        return 0;
    }
    try {
        size_t idx = 0;
        int v = stoi(s, &idx);
        string rest = trim(s.substr(idx));
        if (!rest.empty()) errors.push_back("Unexpected trailing text in " + what + ": '" + raw + "'");
        return v;
    } catch (const exception &) {
        errors.push_back("Invalid integer value for " + what + ": '" + raw + "'");
        return 0;
    }
}

static vector<string> splitList(const string &raw) {
    vector<string> out;
    string token;
    stringstream ss(raw);
    while (getline(ss, token, ',')) {
        token = trim(token);
        if (!token.empty()) out.push_back(token);
    }
    return out;
}

static vector<int> splitIntList(const string &raw, const string &what, vector<string> &errors) {
    vector<int> out;
    for (const string &tok : splitList(raw)) out.push_back(parseIntStrict(tok, what, errors));
    return out;
}

static vector<double> splitDoubleList(const string &raw, const string &what, vector<string> &errors) {
    vector<double> out;
    for (const string &tok : splitList(raw)) out.push_back(parseDoubleStrict(tok, what, errors));
    return out;
}

static double parsePercent(const string &raw, const string &what, vector<string> &errors) {
    string s = trim(raw);
    bool has_percent = false;
    if (!s.empty() && s.back() == '%') {
        has_percent = true;
        s.pop_back();
    }
    double v = parseDoubleStrict(s, what, errors);
    return has_percent ? (v / 100.0) : v;
}

static string fmt(double x) {
    if (fabs(x) < 1e-9) x = 0.0;
    ostringstream os;
    os << fixed << setprecision(6) << x;
    string s = os.str();
    while (s.size() > 1 && s.back() == '0') s.pop_back();
    if (!s.empty() && s.back() == '.') s.pop_back();
    return s;
}


struct Block {
    string name;
    double area = 0.0;
    double width = 0.0;
    double height = 0.0;
    bool has_width = false;
    bool has_height = false;
    double ar_min = 0.0;
    double ar_max = 0.0;
    bool has_ar = false;
    string type; // EDGE, MACRO, SOFT
    vector<string> locations;
    vector<int> port_edges;
    vector<double> ft_conversion;
};

struct Connection {
    int from = -1;
    int to = -1;
    int nets = 0;
};

struct InputData {
    vector<Block> blocks;
    double outline_width = 0.0;
    double outline_height = 0.0;
    double alpha = 0.0;
    vector<vector<int>> conn;
    vector<Connection> connections;
    vector<string> warnings;
    vector<string> errors;
};

struct BlockShape {
    double width = 0.0;
    double height = 0.0;
    double area = 0.0;
    double aspect = 0.0;
    bool area_ok = false;
    bool aspect_ok = false;
    bool fixed_dim_ok = false;
    bool single_outline_fit_ok = false;
};

struct ShapeResult {
    vector<BlockShape> shapes;
    vector<string> warnings;
    vector<string> errors;
};

struct Rect {
    string name;
    double lx = 0.0;
    double ly = 0.0;
    double w = 0.0;
    double h = 0.0;
};

struct EdgePlacement {
    bool placed = false;
    string selected_location;
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
    vector<string> tried_locations;
};

struct EdgePlacementResult {
    vector<EdgePlacement> placements; // same index as blocks
    vector<int> placement_order;
    vector<string> warnings;
    vector<string> errors;
};

struct FullBlockPlacement {
    bool placed = false;
    string method; // EDGE_FIXED, INTERIOR_PACKED, etc.
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
};

struct FullPlacementResult {
    vector<FullBlockPlacement> placements; // same index as blocks
    vector<int> placement_order;
    string strategy_name;
    vector<string> warnings;
    vector<string> errors;
};

struct ChannelRect {
    string name;
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
    int strip_index = -1;
};

struct ChannelGenerationResult {
    vector<ChannelRect> channels;
    double channel_area = 0.0;
    double expected_free_area = 0.0;
    vector<string> warnings;
    vector<string> errors;
};

static string detectType(const string &raw) {
    string t = upperNoSpace(raw);
    if (t.find("SOFT") != string::npos) return "SOFT";
    if (t.find("EDGE") != string::npos) return "EDGE";
    if (t.find("MACRO") != string::npos || t.find("HARD") != string::npos) return "MACRO";
    return "";
}

static int findRecordByFirstCell(const vector<vector<string>> &rec, const string &needleUpperNoSpace, int start = 0) {
    for (int i = start; i < static_cast<int>(rec.size()); ++i) {
        if (upperNoSpace(cell(rec[i], 0)) == needleUpperNoSpace) return i;
    }
    return -1;
}

static int findRecordContainingFirstCell(const vector<vector<string>> &rec, const string &needleUpperNoSpace, int start = 0) {
    for (int i = start; i < static_cast<int>(rec.size()); ++i) {
        string v = upperNoSpace(cell(rec[i], 0));
        if (v.find(needleUpperNoSpace) != string::npos) return i;
    }
    return -1;
}

static InputData parseInputCsv(const string &filename) {
    InputData data;
    vector<vector<string>> rec;
    try {
        rec = readCsvRecords(filename);
    } catch (const exception &e) {
        data.errors.push_back(e.what());
        return data;
    }

    int blockHeader = findRecordByFirstCell(rec, "BLOCK");
    if (blockHeader < 0) {
        data.errors.push_back("Cannot find BLOCK section.");
        return data;
    }

    for (int i = blockHeader + 1; i < static_cast<int>(rec.size()); ++i) {
        string first = upperNoSpace(cell(rec[i], 0));
        if (first == "OUTLINE") break;
        if (isEmptyRecord(rec[i])) continue;
        if (cell(rec[i], 0).empty()) continue;
        if (first.rfind("BLK", 0) != 0) continue;

        Block b;
        b.name = cell(rec[i], 0);
        b.area = parseDoubleStrict(cell(rec[i], 1), b.name + ".AREA", data.errors);
        if (!cell(rec[i], 2).empty()) {
            b.width = parseDoubleStrict(cell(rec[i], 2), b.name + ".WIDTH", data.errors);
            b.has_width = true;
        }
        if (!cell(rec[i], 3).empty()) {
            b.height = parseDoubleStrict(cell(rec[i], 3), b.name + ".HEIGHT", data.errors);
            b.has_height = true;
        }
        if (!cell(rec[i], 4).empty()) {
            vector<double> ar = splitDoubleList(cell(rec[i], 4), b.name + ".ASPECT_RATIO_RANGE", data.errors);
            if (ar.size() == 1) {
                b.ar_min = b.ar_max = ar[0];
                b.has_ar = true;
            } else if (ar.size() >= 2) {
                b.ar_min = min(ar[0], ar[1]);
                b.ar_max = max(ar[0], ar[1]);
                b.has_ar = true;
            } else {
                data.errors.push_back("Invalid aspect ratio range for " + b.name);
            }
        }
        b.type = detectType(cell(rec[i], 5));
        if (b.type.empty()) data.errors.push_back("Cannot detect block type for " + b.name + ": '" + cell(rec[i], 5) + "'");
        b.locations = splitList(cell(rec[i], 6));
        b.port_edges = splitIntList(cell(rec[i], 7), b.name + ".PORT_EDGE", data.errors);
        for (int c = 8; c < 12; ++c) {
            if (!cell(rec[i], c).empty()) b.ft_conversion.push_back(parsePercent(cell(rec[i], c), b.name + ".FT_CONVERSION", data.errors));
        }
        data.blocks.push_back(b);
    }

    if (data.blocks.empty()) data.errors.push_back("No block rows found.");

    map<string, int> blockIndex;
    for (int i = 0; i < static_cast<int>(data.blocks.size()); ++i) {
        if (blockIndex.count(data.blocks[i].name)) data.errors.push_back("Duplicate block name: " + data.blocks[i].name);
        blockIndex[data.blocks[i].name] = i;
    }

    int outlineRow = findRecordByFirstCell(rec, "OUTLINE");
    if (outlineRow < 0) {
        data.errors.push_back("Cannot find OUTLINE section.");
    } else {
        int maxRow = -1;
        for (int i = outlineRow + 1; i < static_cast<int>(rec.size()); ++i) {
            if (upperNoSpace(cell(rec[i], 0)) == "MAX") {
                maxRow = i;
                break;
            }
            if (upperNoSpace(cell(rec[i], 0)) == "CONNMATRIX") break;
        }
        if (maxRow < 0) {
            data.errors.push_back("Cannot find MAX row in OUTLINE section.");
        } else {
            data.outline_width = parseDoubleStrict(cell(rec[maxRow], 1), "OUTLINE.MAX.WIDTH", data.errors);
            data.outline_height = parseDoubleStrict(cell(rec[maxRow], 2), "OUTLINE.MAX.HEIGHT", data.errors);
        }
    }

    bool foundAlpha = false;
    for (int i = 0; i < static_cast<int>(rec.size()); ++i) {
        string first = cell(rec[i], 0);
        string firstUpper = upperNoSpace(first);
        if (first == "α" || firstUpper == "ALPHA" || firstUpper == "A") {
            data.alpha = parseDoubleStrict(cell(rec[i], 1), "alpha", data.errors);
            foundAlpha = true;
            break;
        }
    }
    if (!foundAlpha) data.errors.push_back("Cannot find alpha row.");

    int matrixTitle = findRecordContainingFirstCell(rec, "ONCHIPINTERFACECONNECTION", 0);
    if (matrixTitle < 0) {
        data.errors.push_back("Cannot find connection matrix title row.");
    } else {
        int headerRow = -1;
        for (int i = matrixTitle + 1; i < static_cast<int>(rec.size()); ++i) {
            if (isEmptyRecord(rec[i])) continue;
            if (cell(rec[i], 0).empty() && rec[i].size() >= data.blocks.size() + 1) {
                headerRow = i;
                break;
            }
        }
        if (headerRow < 0) {
            data.errors.push_back("Cannot find connection matrix column header row.");
        } else {
            int n = static_cast<int>(data.blocks.size());
            for (int j = 0; j < n; ++j) {
                string colName = cell(rec[headerRow], j + 1);
                if (colName != data.blocks[j].name) {
                    data.errors.push_back("Matrix column " + to_string(j + 1) + " is '" + colName + "', expected '" + data.blocks[j].name + "'.");
                }
            }
            data.conn.assign(n, vector<int>(n, 0));
            int parsedRows = 0;
            for (int i = headerRow + 1; i < static_cast<int>(rec.size()) && parsedRows < n; ++i) {
                if (isEmptyRecord(rec[i])) continue;
                string rowName = cell(rec[i], 0);
                if (rowName.empty()) continue;
                if (blockIndex.find(rowName) == blockIndex.end()) {
                    data.errors.push_back("Unknown matrix row block name: '" + rowName + "'.");
                    ++parsedRows;
                    continue;
                }
                int rowIndex = blockIndex[rowName];
                if (rowName != data.blocks[parsedRows].name) {
                    data.errors.push_back("Matrix row " + to_string(parsedRows + 1) + " is '" + rowName + "', expected '" + data.blocks[parsedRows].name + "'.");
                }
                for (int j = 0; j < n; ++j) {
                    int val = parseIntStrict(cell(rec[i], j + 1), "CONN[" + rowName + "][" + data.blocks[j].name + "]", data.errors);
                    if (val < 0) data.errors.push_back("Negative connection count at " + rowName + " -> " + data.blocks[j].name);
                    data.conn[rowIndex][j] = val;
                    if (val > 0) data.connections.push_back({rowIndex, j, val});
                }
                ++parsedRows;
            }
            if (parsedRows != n) data.errors.push_back("Connection matrix row count mismatch.");
        }
    }

    for (const Block &b : data.blocks) {
        if (b.area <= 0) data.errors.push_back(b.name + " has non-positive AREA.");
        if (b.type == "SOFT") {
            if (!b.has_ar) data.errors.push_back(b.name + " is SOFT but has no aspect ratio range.");
            if (b.has_ar && (b.ar_min <= 0 || b.ar_max <= 0 || b.ar_min > b.ar_max)) data.errors.push_back(b.name + " has invalid aspect ratio range.");
        } else if (b.type == "EDGE" || b.type == "MACRO") {
            if (!b.has_width || !b.has_height) data.errors.push_back(b.name + " is " + b.type + " but fixed WIDTH/HEIGHT is missing.");
        }
        if (b.type == "EDGE" && b.locations.empty()) data.errors.push_back(b.name + " is EDGE but LOCATION is empty.");
        if (b.port_edges.empty()) data.warnings.push_back(b.name + " has empty PORT EDGE; later routing may choose a legal side automatically.");
        for (int e : b.port_edges) {
            if (e < 0 || e > 4) data.errors.push_back(b.name + " has invalid PORT EDGE " + to_string(e) + "; expected 0~4 from input.");
        }
        if (b.ft_conversion.size() != 4) data.errors.push_back(b.name + " should have 4 FT conversion values, found " + to_string(b.ft_conversion.size()) + ".");
    }
    if (data.outline_width <= 0 || data.outline_height <= 0) data.errors.push_back("Outline MAX width/height must be positive.");
    if (data.alpha < 0) data.errors.push_back("Alpha must be non-negative.");

    return data;
}

static bool nearlyEqualRel(double a, double b, double relTol = AREA_REL_TOL) {
    double denom = max(1.0, max(fabs(a), fabs(b)));
    return fabs(a - b) <= relTol * denom;
}

static ShapeResult generateLegalBlockShapes(const InputData &d) {
    ShapeResult sr;
    sr.shapes.assign(d.blocks.size(), BlockShape{});

    for (size_t i = 0; i < d.blocks.size(); ++i) {
        const Block &b = d.blocks[i];
        BlockShape s;

        if (b.type == "EDGE" || b.type == "MACRO") {
            s.width = b.width;
            s.height = b.height;
            s.area = s.width * s.height;
            s.aspect = (s.height > 0.0 ? s.width / s.height : 0.0);
            s.fixed_dim_ok = b.has_width && b.has_height && s.width > 0.0 && s.height > 0.0;
            s.area_ok = nearlyEqualRel(s.area, b.area);
            s.aspect_ok = true;
            if (!s.area_ok) {
                sr.warnings.push_back(b.name + " fixed WIDTH*HEIGHT=" + fmt(s.area) + " differs from AREA=" + fmt(b.area) + ". The fixed dimensions are still used.");
            }
        } else if (b.type == "SOFT") {
            double targetAspect = 1.0;
            if (b.has_ar) targetAspect = min(max(targetAspect, b.ar_min), b.ar_max);
            s.width = sqrt(b.area * targetAspect);
            s.height = sqrt(b.area / targetAspect);
            s.area = s.width * s.height;
            s.aspect = (s.height > 0.0 ? s.width / s.height : 0.0);
            s.fixed_dim_ok = true;
            s.area_ok = nearlyEqualRel(s.area, b.area);
            s.aspect_ok = b.has_ar && s.aspect + EPS >= b.ar_min && s.aspect <= b.ar_max + EPS;
        } else {
            sr.errors.push_back(b.name + " has unknown block type; cannot generate shape.");
        }

        s.single_outline_fit_ok = (s.width <= d.outline_width + EPS && s.height <= d.outline_height + EPS);
        if (!s.area_ok) sr.errors.push_back(b.name + " shape area check failed: generated=" + fmt(s.area) + " input=" + fmt(b.area));
        if (!s.aspect_ok) sr.errors.push_back(b.name + " aspect ratio check failed: aspect=" + fmt(s.aspect));
        if (!s.fixed_dim_ok) sr.errors.push_back(b.name + " fixed dimension check failed.");
        if (!s.single_outline_fit_ok) sr.errors.push_back(b.name + " generated shape cannot fit inside outline individually.");

        sr.shapes[i] = s;
    }

    return sr;
}

static bool rectsOverlap(double ax, double ay, double aw, double ah, double bx, double by, double bw, double bh) {
    if (ax + aw <= bx + EPS) return false;
    if (bx + bw <= ax + EPS) return false;
    if (ay + ah <= by + EPS) return false;
    if (by + bh <= ay + EPS) return false;
    return true;
}

static bool insideOutline(double lx, double ly, double w, double h, double outlineW, double outlineH) {
    return lx >= -EPS && ly >= -EPS && lx + w <= outlineW + EPS && ly + h <= outlineH + EPS;
}

struct LocInfo {
    bool valid = false;
    char side = '?'; // T, B, L, R
    char region = '?'; // L/M/R for top/bottom; T/M/B for left/right
    double low = 0.0;  // center interval low
    double high = 0.0; // center interval high
};

static LocInfo decodeLocation(const string &locRaw, double outlineW, double outlineH) {
    string loc = upperNoSpace(locRaw);
    LocInfo info;
    if (loc.size() != 2) return info;

    char a = loc[0];
    char b = loc[1];
    if ((a == 'T' || a == 'B') && (b == 'L' || b == 'M' || b == 'R')) {
        info.valid = true;
        info.side = a;
        info.region = b;
        double third = outlineW / 3.0;
        if (b == 'L') { info.low = 0.0; info.high = third; }
        else if (b == 'M') { info.low = third; info.high = 2.0 * third; }
        else { info.low = 2.0 * third; info.high = outlineW; }
    } else if ((a == 'L' || a == 'R') && (b == 'T' || b == 'M' || b == 'B')) {
        info.valid = true;
        info.side = a;
        info.region = b;
        double third = outlineH / 3.0;
        if (b == 'B') { info.low = 0.0; info.high = third; }
        else if (b == 'M') { info.low = third; info.high = 2.0 * third; }
        else { info.low = 2.0 * third; info.high = outlineH; }
    }
    return info;
}

static void addUniqueCandidate(vector<double> &vals, double v) {
    for (double old : vals) {
        if (fabs(old - v) <= 1e-4) return;
    }
    vals.push_back(v);
}

static vector<Rect> generateCandidatesForLocation(const string &blockName, const string &locRaw, double w, double h, double outlineW, double outlineH) {
    vector<Rect> out;
    LocInfo info = decodeLocation(locRaw, outlineW, outlineH);
    if (!info.valid) return out;

    double maxPos = 0.0;
    double minCoord = 0.0;
    double maxCoord = 0.0;
    vector<double> coords;

    if (info.side == 'T' || info.side == 'B') {
        maxPos = outlineW - w;
        minCoord = max(0.0, info.low - w / 2.0);
        maxCoord = min(maxPos, info.high - w / 2.0);
        double centerCoord = (info.low + info.high) / 2.0 - w / 2.0;
        addUniqueCandidate(coords, min(max(0.0, centerCoord), maxPos));
        addUniqueCandidate(coords, minCoord);
        addUniqueCandidate(coords, maxCoord);
        addUniqueCandidate(coords, 0.0);
        addUniqueCandidate(coords, maxPos);
        if (minCoord <= maxCoord + EPS) {
            for (int k = 0; k <= 40; ++k) {
                double t = static_cast<double>(k) / 40.0;
                addUniqueCandidate(coords, minCoord * (1.0 - t) + maxCoord * t);
            }
        }
        for (double x : coords) {
            Rect r;
            r.name = blockName;
            r.lx = x;
            r.ly = (info.side == 'T') ? (outlineH - h) : 0.0;
            r.w = w;
            r.h = h;
            out.push_back(r);
        }
    } else {
        maxPos = outlineH - h;
        minCoord = max(0.0, info.low - h / 2.0);
        maxCoord = min(maxPos, info.high - h / 2.0);
        double centerCoord = (info.low + info.high) / 2.0 - h / 2.0;
        addUniqueCandidate(coords, min(max(0.0, centerCoord), maxPos));
        addUniqueCandidate(coords, minCoord);
        addUniqueCandidate(coords, maxCoord);
        addUniqueCandidate(coords, 0.0);
        addUniqueCandidate(coords, maxPos);
        if (minCoord <= maxCoord + EPS) {
            for (int k = 0; k <= 40; ++k) {
                double t = static_cast<double>(k) / 40.0;
                addUniqueCandidate(coords, minCoord * (1.0 - t) + maxCoord * t);
            }
        }
        for (double y : coords) {
            Rect r;
            r.name = blockName;
            r.lx = (info.side == 'R') ? (outlineW - w) : 0.0;
            r.ly = y;
            r.w = w;
            r.h = h;
            out.push_back(r);
        }
    }

    return out;
}

static bool locationSatisfied(const Rect &r, const string &locRaw, double outlineW, double outlineH) {
    LocInfo info = decodeLocation(locRaw, outlineW, outlineH);
    if (!info.valid) return false;
    if (!insideOutline(r.lx, r.ly, r.w, r.h, outlineW, outlineH)) return false;

    double cx = r.lx + r.w / 2.0;
    double cy = r.ly + r.h / 2.0;
    bool touches = false;
    bool centerInRegion = false;

    if (info.side == 'T') {
        touches = fabs((r.ly + r.h) - outlineH) <= 1e-4;
        centerInRegion = cx + EPS >= info.low && cx <= info.high + EPS;
    } else if (info.side == 'B') {
        touches = fabs(r.ly) <= 1e-4;
        centerInRegion = cx + EPS >= info.low && cx <= info.high + EPS;
    } else if (info.side == 'L') {
        touches = fabs(r.lx) <= 1e-4;
        centerInRegion = cy + EPS >= info.low && cy <= info.high + EPS;
    } else if (info.side == 'R') {
        touches = fabs((r.lx + r.w) - outlineW) <= 1e-4;
        centerInRegion = cy + EPS >= info.low && cy <= info.high + EPS;
    }
    return touches && centerInRegion;
}

static bool overlapsAnyPlacedEdge(const Rect &candidate, const vector<Rect> &placed) {
    for (const Rect &p : placed) {
        if (rectsOverlap(candidate.lx, candidate.ly, candidate.w, candidate.h, p.lx, p.ly, p.w, p.h)) return true;
    }
    return false;
}

static EdgePlacementResult placeEdgeBlocksFirst(const InputData &d, const ShapeResult &sr) {
    EdgePlacementResult er;
    er.placements.assign(d.blocks.size(), EdgePlacement{});

    vector<int> edgeIdx;
    for (int i = 0; i < static_cast<int>(d.blocks.size()); ++i) {
        if (d.blocks[i].type == "EDGE") edgeIdx.push_back(i);
    }

    sort(edgeIdx.begin(), edgeIdx.end(), [&](int a, int b) {
        double areaA = sr.shapes[a].width * sr.shapes[a].height;
        double areaB = sr.shapes[b].width * sr.shapes[b].height;
        if (fabs(areaA - areaB) > EPS) return areaA > areaB;
        return d.blocks[a].name < d.blocks[b].name;
    });

    vector<Rect> placed;
    for (int idx : edgeIdx) {
        const Block &b = d.blocks[idx];
        const BlockShape &s = sr.shapes[idx];
        EdgePlacement ep;
        ep.width = s.width;
        ep.height = s.height;
        bool done = false;

        for (const string &loc : b.locations) {
            ep.tried_locations.push_back(loc);
            vector<Rect> candidates = generateCandidatesForLocation(b.name, loc, s.width, s.height, d.outline_width, d.outline_height);

            vector<Rect> feasible;
            for (const Rect &c : candidates) {
                if (!locationSatisfied(c, loc, d.outline_width, d.outline_height)) continue;
                if (overlapsAnyPlacedEdge(c, placed)) continue;
                feasible.push_back(c);
            }
            if (!feasible.empty()) {
                // Prefer positions close to the region center while still avoiding overlap.
                LocInfo info = decodeLocation(loc, d.outline_width, d.outline_height);
                double target = (info.low + info.high) / 2.0;
                sort(feasible.begin(), feasible.end(), [&](const Rect &a, const Rect &bRect) {
                    double ca = (info.side == 'T' || info.side == 'B') ? (a.lx + a.w / 2.0) : (a.ly + a.h / 2.0);
                    double cb = (info.side == 'T' || info.side == 'B') ? (bRect.lx + bRect.w / 2.0) : (bRect.ly + bRect.h / 2.0);
                    double da = fabs(ca - target);
                    double db = fabs(cb - target);
                    if (fabs(da - db) > 1e-4) return da < db;
                    if (fabs(a.lx - bRect.lx) > 1e-4) return a.lx < bRect.lx;
                    return a.ly < bRect.ly;
                });
                const Rect chosen = feasible.front();
                ep.placed = true;
                ep.selected_location = loc;
                ep.lx = chosen.lx;
                ep.ly = chosen.ly;
                placed.push_back(chosen);
                er.placement_order.push_back(idx);
                done = true;
                break;
            }
        }

        if (!done) {
            er.errors.push_back(b.name + " cannot be placed on any allowed EDGE LOCATION without overlap.");
        }
        er.placements[idx] = ep;
    }

    return er;
}


static Rect rectFromFullPlacement(const string &name, const FullBlockPlacement &p) {
    Rect r;
    r.name = name;
    r.lx = p.lx;
    r.ly = p.ly;
    r.w = p.width;
    r.h = p.height;
    return r;
}

static bool overlapsAnyRect(const Rect &candidate, const vector<Rect> &placed) {
    for (const Rect &p : placed) {
        if (rectsOverlap(candidate.lx, candidate.ly, candidate.w, candidate.h, p.lx, p.ly, p.w, p.h)) return true;
    }
    return false;
}

static bool overlapsAnyRectWithGap(const Rect &candidate, const vector<Rect> &placed, double gap) {
    for (const Rect &p : placed) {
        if (rectsOverlap(candidate.lx, candidate.ly, candidate.w, candidate.h,
                         p.lx - gap, p.ly - gap, p.w + 2.0 * gap, p.h + 2.0 * gap)) return true;
    }
    return false;
}

static void addClampedCandidate(vector<double> &vals, double v, double lo, double hi) {
    if (hi < lo - EPS) return;
    v = min(max(v, lo), hi);
    addUniqueCandidate(vals, v);
}

static vector<Rect> generateInteriorPlacementCandidates(const string &name,
                                                         double w,
                                                         double h,
                                                         double outlineW,
                                                         double outlineH,
                                                         const vector<Rect> &placed,
                                                         double routingGap = 20.0) {
    vector<Rect> candidates;
    if (w <= 0.0 || h <= 0.0 || w > outlineW + EPS || h > outlineH + EPS) return candidates;

    // Keep a whitespace moat around every non-edge block placement.
    // Step 13 can pass a larger demand-aware gap for high-traffic blocks.
    routingGap = max(0.0, routingGap);
    double loX = routingGap;
    double loY = routingGap;
    double hiX = outlineW - w - routingGap;
    double hiY = outlineH - h - routingGap;

    // If a very tight future testcase cannot afford this gap, fall back to no boundary gap.
    if (hiX < loX - EPS) { loX = 0.0; hiX = outlineW - w; }
    if (hiY < loY - EPS) { loY = 0.0; hiY = outlineH - h; }

    vector<double> xs, ys;

    addClampedCandidate(xs, loX, loX, hiX);
    addClampedCandidate(xs, hiX, loX, hiX);
    addClampedCandidate(xs, outlineW / 3.0 - w / 2.0, loX, hiX);
    addClampedCandidate(xs, outlineW / 2.0 - w / 2.0, loX, hiX);
    addClampedCandidate(xs, 2.0 * outlineW / 3.0 - w / 2.0, loX, hiX);

    addClampedCandidate(ys, loY, loY, hiY);
    addClampedCandidate(ys, hiY, loY, hiY);
    addClampedCandidate(ys, outlineH / 3.0 - h / 2.0, loY, hiY);
    addClampedCandidate(ys, outlineH / 2.0 - h / 2.0, loY, hiY);
    addClampedCandidate(ys, 2.0 * outlineH / 3.0 - h / 2.0, loY, hiY);

    for (const Rect &r : placed) {
        // Critical coordinates with an explicit routing gap around already placed rectangles.
        addClampedCandidate(xs, r.lx + r.w + routingGap, loX, hiX);
        addClampedCandidate(xs, r.lx - w - routingGap, loX, hiX);
        addClampedCandidate(xs, r.lx, loX, hiX);
        addClampedCandidate(xs, r.lx + r.w - w, loX, hiX);
        addClampedCandidate(xs, r.lx + r.w / 2.0 - w / 2.0, loX, hiX);

        addClampedCandidate(ys, r.ly + r.h + routingGap, loY, hiY);
        addClampedCandidate(ys, r.ly - h - routingGap, loY, hiY);
        addClampedCandidate(ys, r.ly, loY, hiY);
        addClampedCandidate(ys, r.ly + r.h - h, loY, hiY);
        addClampedCandidate(ys, r.ly + r.h / 2.0 - h / 2.0, loY, hiY);
    }

    sort(xs.begin(), xs.end());
    sort(ys.begin(), ys.end());

    // First try with the routing gap. If impossible, try exact non-overlap as a fallback.
    for (double y : ys) {
        for (double x : xs) {
            Rect c{name, x, y, w, h};
            if (!insideOutline(c.lx, c.ly, c.w, c.h, outlineW, outlineH)) continue;
            if (overlapsAnyRectWithGap(c, placed, routingGap)) continue;
            candidates.push_back(c);
        }
    }

    if (candidates.empty()) {
        for (double y : ys) {
            for (double x : xs) {
                Rect c{name, x, y, w, h};
                if (!insideOutline(c.lx, c.ly, c.w, c.h, outlineW, outlineH)) continue;
                if (overlapsAnyRect(c, placed)) continue;
                candidates.push_back(c);
            }
        }
    }

    return candidates;
}

static double connectivityWeightToPlaced(const InputData &d, int idx, const vector<Rect> &placedByIndex) {
    double weight = 0.0;
    for (size_t j = 0; j < d.blocks.size(); ++j) {
        if (idx == static_cast<int>(j)) continue;
        if (placedByIndex[j].name.empty()) continue;
        if (idx < static_cast<int>(d.conn.size()) && j < d.conn[idx].size()) weight += d.conn[idx][j];
        if (j < d.conn.size() && idx < static_cast<int>(d.conn[j].size())) weight += d.conn[j][idx];
    }
    return weight;
}

static double manhattanCenterDistance(const Rect &a, const Rect &b) {
    double ax = a.lx + a.w / 2.0;
    double ay = a.ly + a.h / 2.0;
    double bx = b.lx + b.w / 2.0;
    double by = b.ly + b.h / 2.0;
    return fabs(ax - bx) + fabs(ay - by);
}

static double candidateConnectivityScore(const InputData &d, int idx, const Rect &candidate, const vector<Rect> &placedByIndex) {
    double score = 0.0;
    for (size_t j = 0; j < d.blocks.size(); ++j) {
        if (idx == static_cast<int>(j)) continue;
        if (placedByIndex[j].name.empty()) continue;
        double nets = 0.0;
        if (idx < static_cast<int>(d.conn.size()) && j < d.conn[idx].size()) nets += d.conn[idx][j];
        if (j < d.conn.size() && idx < static_cast<int>(d.conn[j].size())) nets += d.conn[j][idx];
        if (nets > 0.0) score += nets * manhattanCenterDistance(candidate, placedByIndex[j]);
    }
    return score;
}


struct PlacementStrategyConfig {
    string name = "BASELINE_PLACEMENT";
    bool demand_aware = false;
    double base_gap = 20.0;
    double extra_gap = 0.0;
    double connectivity_weight = 1.0;
    double center_weight = 0.0;
    double compact_weight = 1.0;
};

static double totalBlockDemand(const InputData &d, int idx) {
    double demand = 0.0;
    for (size_t j = 0; j < d.blocks.size(); ++j) {
        if (idx == static_cast<int>(j)) continue;
        if (idx < static_cast<int>(d.conn.size()) && j < d.conn[idx].size()) demand += d.conn[idx][j];
        if (j < d.conn.size() && idx < static_cast<int>(d.conn[j].size())) demand += d.conn[j][idx];
    }
    return demand;
}

static vector<double> computeBlockDemands(const InputData &d) {
    vector<double> demand(d.blocks.size(), 0.0);
    for (int i = 0; i < static_cast<int>(d.blocks.size()); ++i) demand[i] = totalBlockDemand(d, i);
    return demand;
}

static double maxDemandValue(const vector<double> &demand) {
    double m = 0.0;
    for (double v : demand) m = max(m, v);
    return m;
}

static double candidateCenterBiasScore(const Rect &candidate, double outlineW, double outlineH) {
    double cx = candidate.lx + candidate.w / 2.0;
    double cy = candidate.ly + candidate.h / 2.0;
    return fabs(cx - outlineW / 2.0) + fabs(cy - outlineH / 2.0);
}

static double candidateCompactScore(const Rect &candidate) {
    double top = candidate.ly + candidate.h;
    double right = candidate.lx + candidate.w;
    return top + 0.35 * right;
}

static FullPlacementResult placeAllBlocksAfterEdgesWithStrategy(const InputData &d,
                                                                const ShapeResult &sr,
                                                                const EdgePlacementResult &er,
                                                                const PlacementStrategyConfig &cfg) {
    FullPlacementResult pr;
    pr.placements.assign(d.blocks.size(), FullBlockPlacement{});
    pr.strategy_name = cfg.name;

    vector<Rect> placed;
    vector<Rect> placedByIndex(d.blocks.size());
    vector<double> demand = computeBlockDemands(d);
    double maxDemand = maxDemandValue(demand);

    // Copy EDGE placements from the previous step.
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (d.blocks[i].type != "EDGE") continue;
        const EdgePlacement &ep = er.placements[i];
        FullBlockPlacement bp;
        bp.placed = ep.placed;
        bp.method = "EDGE_FIXED";
        bp.lx = ep.lx;
        bp.ly = ep.ly;
        bp.width = ep.width;
        bp.height = ep.height;
        pr.placements[i] = bp;
        if (bp.placed) {
            Rect r = rectFromFullPlacement(d.blocks[i].name, bp);
            placed.push_back(r);
            placedByIndex[i] = r;
            pr.placement_order.push_back(static_cast<int>(i));
        }
    }

    vector<int> innerIdx;
    for (int i = 0; i < static_cast<int>(d.blocks.size()); ++i) {
        if (d.blocks[i].type != "EDGE") innerIdx.push_back(i);
    }

    sort(innerIdx.begin(), innerIdx.end(), [&](int a, int b) {
        bool am = d.blocks[a].type == "MACRO";
        bool bm = d.blocks[b].type == "MACRO";
        if (am != bm) return am > bm;
        if (cfg.demand_aware && fabs(demand[a] - demand[b]) > EPS) return demand[a] > demand[b];
        double areaA = sr.shapes[a].width * sr.shapes[a].height;
        double areaB = sr.shapes[b].width * sr.shapes[b].height;
        if (fabs(areaA - areaB) > EPS) return areaA > areaB;
        double maxSideA = max(sr.shapes[a].width, sr.shapes[a].height);
        double maxSideB = max(sr.shapes[b].width, sr.shapes[b].height);
        if (fabs(maxSideA - maxSideB) > EPS) return maxSideA > maxSideB;
        return d.blocks[a].name < d.blocks[b].name;
    });

    for (int idx : innerIdx) {
        const Block &b = d.blocks[idx];
        const BlockShape &s = sr.shapes[idx];
        double normDemand = (maxDemand > EPS ? demand[idx] / maxDemand : 0.0);
        double dynamicGap = cfg.base_gap + cfg.extra_gap * sqrt(max(0.0, normDemand));

        vector<double> gapTrials;
        gapTrials.push_back(dynamicGap);
        gapTrials.push_back(cfg.base_gap);
        gapTrials.push_back(20.0);
        gapTrials.push_back(8.0);
        gapTrials.push_back(0.0);
        vector<Rect> candidates;
        double usedGap = dynamicGap;
        for (double g : gapTrials) {
            candidates = generateInteriorPlacementCandidates(b.name, s.width, s.height,
                                                             d.outline_width, d.outline_height,
                                                             placed, g);
            if (!candidates.empty()) { usedGap = g; break; }
        }

        if (candidates.empty()) {
            pr.errors.push_back(b.name + " cannot be placed inside outline without overlap. shape=" + fmt(s.width) + "x" + fmt(s.height));
            continue;
        }

        double connWeight = connectivityWeightToPlaced(d, idx, placedByIndex);
        sort(candidates.begin(), candidates.end(), [&](const Rect &a, const Rect &bRect) {
            if (!cfg.demand_aware) {
                // Preserve the exact Step-12 baseline ordering for a safe fallback.
                if (connWeight > 0.0) {
                    double sa = candidateConnectivityScore(d, idx, a, placedByIndex);
                    double sb = candidateConnectivityScore(d, idx, bRect, placedByIndex);
                    if (fabs(sa - sb) > 1e-4) return sa < sb;
                }
                double topA = a.ly + a.h;
                double topB = bRect.ly + bRect.h;
                if (fabs(topA - topB) > 1e-4) return topA < topB;
                double rightA = a.lx + a.w;
                double rightB = bRect.lx + bRect.w;
                if (fabs(rightA - rightB) > 1e-4) return rightA < rightB;
                if (fabs(a.ly - bRect.ly) > 1e-4) return a.ly < bRect.ly;
                return a.lx < bRect.lx;
            }

            double scoreA = 0.0;
            double scoreB = 0.0;
            if (connWeight > 0.0) {
                scoreA += cfg.connectivity_weight * candidateConnectivityScore(d, idx, a, placedByIndex);
                scoreB += cfg.connectivity_weight * candidateConnectivityScore(d, idx, bRect, placedByIndex);
            }
            scoreA += cfg.center_weight * normDemand * candidateCenterBiasScore(a, d.outline_width, d.outline_height);
            scoreB += cfg.center_weight * normDemand * candidateCenterBiasScore(bRect, d.outline_width, d.outline_height);
            scoreA += cfg.compact_weight * (1.0 - 0.55 * normDemand) * candidateCompactScore(a);
            scoreB += cfg.compact_weight * (1.0 - 0.55 * normDemand) * candidateCompactScore(bRect);
            if (fabs(scoreA - scoreB) > 1e-4) return scoreA < scoreB;
            double topA = a.ly + a.h;
            double topB = bRect.ly + bRect.h;
            if (fabs(topA - topB) > 1e-4) return topA < topB;
            double rightA = a.lx + a.w;
            double rightB = bRect.lx + bRect.w;
            if (fabs(rightA - rightB) > 1e-4) return rightA < rightB;
            if (fabs(a.ly - bRect.ly) > 1e-4) return a.ly < bRect.ly;
            return a.lx < bRect.lx;
        });

        Rect chosen = candidates.front();
        FullBlockPlacement bp;
        bp.placed = true;
        bp.method = cfg.demand_aware ? ("DEMAND_AWARE_GAP_" + fmt(usedGap)) : "INTERIOR_PACKED";
        bp.lx = chosen.lx;
        bp.ly = chosen.ly;
        bp.width = chosen.w;
        bp.height = chosen.h;
        pr.placements[idx] = bp;
        placed.push_back(chosen);
        placedByIndex[idx] = chosen;
        pr.placement_order.push_back(idx);
    }

    pr.warnings.push_back("PLACEMENT_STRATEGY_USED: " + cfg.name);
    return pr;
}

static bool verifyAllInsideOutline(const InputData &d, const FullPlacementResult &pr, vector<string> &errors) {
    bool ok = true;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        const FullBlockPlacement &p = pr.placements[i];
        if (!p.placed) {
            ok = false;
            errors.push_back(d.blocks[i].name + " is not placed.");
            continue;
        }
        if (!insideOutline(p.lx, p.ly, p.width, p.height, d.outline_width, d.outline_height)) {
            ok = false;
            errors.push_back(d.blocks[i].name + " is outside outline.");
        }
    }
    return ok;
}

static bool verifyNoBlockOverlap(const InputData &d, const FullPlacementResult &pr, vector<string> &errors) {
    bool ok = true;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (!pr.placements[i].placed) continue;
        Rect a = rectFromFullPlacement(d.blocks[i].name, pr.placements[i]);
        for (size_t j = i + 1; j < d.blocks.size(); ++j) {
            if (!pr.placements[j].placed) continue;
            Rect b = rectFromFullPlacement(d.blocks[j].name, pr.placements[j]);
            if (rectsOverlap(a.lx, a.ly, a.w, a.h, b.lx, b.ly, b.w, b.h)) {
                ok = false;
                errors.push_back("BLOCK overlap: " + a.name + " overlaps " + b.name);
            }
        }
    }
    return ok;
}

static bool verifyEdgeConstraintsInFullPlacement(const InputData &d, const FullPlacementResult &pr, const EdgePlacementResult &er, vector<string> &errors) {
    bool ok = true;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (d.blocks[i].type != "EDGE") continue;
        const FullBlockPlacement &p = pr.placements[i];
        if (!p.placed) {
            ok = false;
            errors.push_back(d.blocks[i].name + " is EDGE but not placed.");
            continue;
        }
        string selected = er.placements[i].selected_location;
        Rect r = rectFromFullPlacement(d.blocks[i].name, p);
        if (!locationSatisfied(r, selected, d.outline_width, d.outline_height)) {
            ok = false;
            errors.push_back(d.blocks[i].name + " no longer satisfies EDGE location " + selected + ".");
        }
    }
    return ok;
}


static double totalPlacedArea(const InputData &d, const FullPlacementResult &pr);

static vector<Rect> collectPlacedRects(const InputData &d, const FullPlacementResult &pr) {
    vector<Rect> rects;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (!pr.placements[i].placed) continue;
        rects.push_back(rectFromFullPlacement(d.blocks[i].name, pr.placements[i]));
    }
    return rects;
}

static void addUniqueCoord(vector<double> &coords, double v, double lo, double hi) {
    v = min(max(v, lo), hi);
    for (double old : coords) {
        if (fabs(old - v) <= 1e-4) return;
    }
    coords.push_back(v);
}

static vector<pair<double, double>> mergeIntervals(vector<pair<double, double>> intervals) {
    vector<pair<double, double>> merged;
    sort(intervals.begin(), intervals.end());
    for (auto seg : intervals) {
        if (seg.second <= seg.first + EPS) continue;
        if (merged.empty() || seg.first > merged.back().second + EPS) {
            merged.push_back(seg);
        } else {
            merged.back().second = max(merged.back().second, seg.second);
        }
    }
    return merged;
}

static ChannelGenerationResult generateVerticalSliceChannels(const InputData &d, const FullPlacementResult &pr) {
    ChannelGenerationResult cr;
    vector<Rect> blocks = collectPlacedRects(d, pr);

    if (blocks.size() != d.blocks.size()) {
        cr.errors.push_back("Cannot generate channels because not all blocks are placed.");
        return cr;
    }

    vector<double> xs;
    addUniqueCoord(xs, 0.0, 0.0, d.outline_width);
    addUniqueCoord(xs, d.outline_width, 0.0, d.outline_width);
    for (const Rect &b : blocks) {
        addUniqueCoord(xs, b.lx, 0.0, d.outline_width);
        addUniqueCoord(xs, b.lx + b.w, 0.0, d.outline_width);
    }
    sort(xs.begin(), xs.end());

    int channelId = 1;
    for (int k = 0; k + 1 < static_cast<int>(xs.size()); ++k) {
        double left = xs[k];
        double right = xs[k + 1];
        double width = right - left;
        if (width <= EPS) continue;

        vector<pair<double, double>> covered;
        for (const Rect &b : blocks) {
            bool xOverlap = (b.lx < right - EPS) && (b.lx + b.w > left + EPS);
            if (!xOverlap) continue;
            double y0 = min(max(b.ly, 0.0), d.outline_height);
            double y1 = min(max(b.ly + b.h, 0.0), d.outline_height);
            if (y1 > y0 + EPS) covered.push_back({y0, y1});
        }

        vector<pair<double, double>> merged = mergeIntervals(covered);
        double yPrev = 0.0;
        for (const auto &seg : merged) {
            if (seg.first > yPrev + EPS) {
                ChannelRect ch;
                ch.name = "CH" + to_string(channelId++);
                ch.lx = left;
                ch.ly = yPrev;
                ch.width = width;
                ch.height = seg.first - yPrev;
                ch.strip_index = k;
                cr.channel_area += ch.width * ch.height;
                cr.channels.push_back(ch);
            }
            yPrev = max(yPrev, seg.second);
        }
        if (d.outline_height > yPrev + EPS) {
            ChannelRect ch;
            ch.name = "CH" + to_string(channelId++);
            ch.lx = left;
            ch.ly = yPrev;
            ch.width = width;
            ch.height = d.outline_height - yPrev;
            ch.strip_index = k;
            cr.channel_area += ch.width * ch.height;
            cr.channels.push_back(ch);
        }
    }

    double blockArea = totalPlacedArea(d, pr);
    cr.expected_free_area = d.outline_width * d.outline_height - blockArea;
    if (cr.channels.empty() && cr.expected_free_area > EPS) {
        cr.errors.push_back("No channels were generated even though free area exists.");
    }
    return cr;
}

static Rect rectFromChannel(const ChannelRect &c) {
    return Rect{c.name, c.lx, c.ly, c.width, c.height};
}

static bool verifyChannelsPositiveSize(const ChannelGenerationResult &cr, vector<string> &errors) {
    bool ok = true;
    for (const ChannelRect &c : cr.channels) {
        if (c.width <= EPS || c.height <= EPS) {
            ok = false;
            errors.push_back(c.name + " has non-positive size: " + fmt(c.width) + "x" + fmt(c.height));
        }
    }
    return ok;
}

static bool verifyChannelsInsideOutline(const InputData &d, const ChannelGenerationResult &cr, vector<string> &errors) {
    bool ok = true;
    for (const ChannelRect &c : cr.channels) {
        if (!insideOutline(c.lx, c.ly, c.width, c.height, d.outline_width, d.outline_height)) {
            ok = false;
            errors.push_back(c.name + " is outside outline.");
        }
    }
    return ok;
}

static bool verifyChannelsDoNotOverlapBlocks(const InputData &d, const FullPlacementResult &pr, const ChannelGenerationResult &cr, vector<string> &errors) {
    bool ok = true;
    vector<Rect> blocks = collectPlacedRects(d, pr);
    for (const ChannelRect &ch : cr.channels) {
        Rect c = rectFromChannel(ch);
        for (const Rect &b : blocks) {
            if (rectsOverlap(c.lx, c.ly, c.w, c.h, b.lx, b.ly, b.w, b.h)) {
                ok = false;
                errors.push_back(c.name + " overlaps block " + b.name);
            }
        }
    }
    return ok;
}

static bool verifyChannelsDoNotOverlapEachOther(const ChannelGenerationResult &cr, vector<string> &errors) {
    bool ok = true;
    for (size_t i = 0; i < cr.channels.size(); ++i) {
        Rect a = rectFromChannel(cr.channels[i]);
        for (size_t j = i + 1; j < cr.channels.size(); ++j) {
            Rect b = rectFromChannel(cr.channels[j]);
            if (rectsOverlap(a.lx, a.ly, a.w, a.h, b.lx, b.ly, b.w, b.h)) {
                ok = false;
                errors.push_back(a.name + " overlaps channel " + b.name);
            }
        }
    }
    return ok;
}

static bool verifyChannelAreaCoverage(const InputData &d, const FullPlacementResult &pr, const ChannelGenerationResult &cr, vector<string> &errors) {
    double outlineArea = d.outline_width * d.outline_height;
    double blockArea = totalPlacedArea(d, pr);
    double reconstructed = blockArea + cr.channel_area;
    double diff = fabs(reconstructed - outlineArea);
    double tol = max(1e-3, outlineArea * 1e-7);
    if (diff > tol) {
        errors.push_back("Block area + channel area does not equal outline area. diff=" + fmt(diff));
        return false;
    }
    return true;
}

static int countType(const InputData &d, const string &type) {
    int c = 0;
    for (const Block &b : d.blocks) if (b.type == type) ++c;
    return c;
}

static long long totalDirectedNets(const InputData &d) {
    long long sum = 0;
    for (const Connection &c : d.connections) sum += c.nets;
    return sum;
}


static double totalPlacedArea(const InputData &d, const FullPlacementResult &pr) {
    double sum = 0.0;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (!pr.placements[i].placed) continue;
        sum += pr.placements[i].width * pr.placements[i].height;
    }
    return sum;
}


struct RoutingAdjEdge {
    int to = -1;
    int edge_from = 0;
    int edge_to = 0;
    double gx = 0.0;
    double gy = 0.0;
};

struct PathStep {
    string rect_name;
    int edge = 0;
};

struct RoutePattern {
    int from = -1;
    int to = -1;
    int nets = 0;
    bool routed = false;
    bool used_port_relaxation = false;
    string fail_reason;
    vector<int> nodes; // source block node, channels..., target block node
    vector<PathStep> steps; // output-ready sequence: rect edge rect edge ...
    double estimated_wirelength = 0.0;
};

struct RoutingGenerationResult {
    vector<RoutePattern> routes;
    long long routed_nets = 0;
    long long unrouted_nets = 0;
    int routed_count = 0;
    int unrouted_count = 0;
    int port_relaxed_count = 0;
    double total_estimated_wirelength = 0.0;
    vector<long long> channel_loads; // one value per channel, simple total nets passing through the channel
    vector<long long> soft_ft_loads; // one value per input block; nets feedthroughing intermediate SOFT blocks
    vector<string> warnings;
    vector<string> errors;
};

static bool positiveOverlap(double a1, double a2, double b1, double b2) {
    double lo = max(a1, b1);
    double hi = min(a2, b2);
    return hi - lo > EPS;
}

static pair<double, double> overlapMidpoint(double a1, double a2, double b1, double b2) {
    double lo = max(a1, b1);
    double hi = min(a2, b2);
    return {(lo + hi) / 2.0, hi - lo};
}

static bool adjacentRectsForRouting(const Rect &a, const Rect &b, int &edgeA, int &edgeB, double &gx, double &gy) {
    // Output/PATH edge numbering from the statement: left=1, top=2, right=3, bottom=4.
    double ar = a.lx + a.w;
    double at = a.ly + a.h;
    double br = b.lx + b.w;
    double bt = b.ly + b.h;

    if (fabs(ar - b.lx) <= 1e-5 && positiveOverlap(a.ly, at, b.ly, bt)) {
        auto mid = overlapMidpoint(a.ly, at, b.ly, bt);
        edgeA = 3;
        edgeB = 1;
        gx = ar;
        gy = mid.first;
        return true;
    }
    if (fabs(br - a.lx) <= 1e-5 && positiveOverlap(a.ly, at, b.ly, bt)) {
        auto mid = overlapMidpoint(a.ly, at, b.ly, bt);
        edgeA = 1;
        edgeB = 3;
        gx = a.lx;
        gy = mid.first;
        return true;
    }
    if (fabs(at - b.ly) <= 1e-5 && positiveOverlap(a.lx, ar, b.lx, br)) {
        auto mid = overlapMidpoint(a.lx, ar, b.lx, br);
        edgeA = 2;
        edgeB = 4;
        gx = mid.first;
        gy = at;
        return true;
    }
    if (fabs(bt - a.ly) <= 1e-5 && positiveOverlap(a.lx, ar, b.lx, br)) {
        auto mid = overlapMidpoint(a.lx, ar, b.lx, br);
        edgeA = 4;
        edgeB = 2;
        gx = mid.first;
        gy = a.ly;
        return true;
    }
    return false;
}

static string nodeName(int node, const InputData &d, const ChannelGenerationResult &cr) {
    int n = static_cast<int>(d.blocks.size());
    if (node < n) return d.blocks[node].name;
    return cr.channels[node - n].name;
}

static bool isChannelNode(int node, int blockCount) {
    return node >= blockCount;
}

static bool edgeAllowedByPortConstraint(const Block &b, int outputEdge) {
    if (b.type == "SOFT") return true;
    if (b.port_edges.empty()) return true;
    for (int e : b.port_edges) {
        if (e == 0) return true;
        // The generated PATH uses the output edge numbering: left=1, top=2, right=3, bottom=4.
        // The testcase port-edge values are used directly here. If a testcase/publisher checker
        // interprets port edge differently, this can be adjusted in one function later.
        if (e == outputEdge) return true;
    }
    return false;
}

static vector<vector<RoutingAdjEdge>> buildRoutingGraph(const InputData &d,
                                                        const FullPlacementResult &pr,
                                                        const ChannelGenerationResult &cr) {
    int nBlocks = static_cast<int>(d.blocks.size());
    int nChannels = static_cast<int>(cr.channels.size());
    int nNodes = nBlocks + nChannels;
    vector<Rect> rects;
    rects.reserve(nNodes);
    for (int i = 0; i < nBlocks; ++i) rects.push_back(rectFromFullPlacement(d.blocks[i].name, pr.placements[i]));
    for (const ChannelRect &ch : cr.channels) rects.push_back(rectFromChannel(ch));

    vector<vector<RoutingAdjEdge>> graph(nNodes);
    for (int i = 0; i < nNodes; ++i) {
        for (int j = i + 1; j < nNodes; ++j) {
            bool iChan = isChannelNode(i, nBlocks);
            bool jChan = isChannelNode(j, nBlocks);
            // Channel-only routing: do not route through block-to-block adjacency.
            if (!iChan && !jChan) continue;

            int ei = 0, ej = 0;
            double gx = 0.0, gy = 0.0;
            if (!adjacentRectsForRouting(rects[i], rects[j], ei, ej, gx, gy)) continue;

            graph[i].push_back({j, ei, ej, gx, gy});
            graph[j].push_back({i, ej, ei, gx, gy});
        }
    }
    return graph;
}

static const RoutingAdjEdge *findGraphEdge(const vector<vector<RoutingAdjEdge>> &graph, int u, int v) {
    for (const RoutingAdjEdge &e : graph[u]) {
        if (e.to == v) return &e;
    }
    return nullptr;
}

static bool bfsRouteOne(const InputData &d,
                        const ChannelGenerationResult &cr,
                        const vector<vector<RoutingAdjEdge>> &graph,
                        int source,
                        int target,
                        bool respectPortEdges,
                        vector<int> &outNodes,
                        string &failReason) {
    int nBlocks = static_cast<int>(d.blocks.size());
    int nNodes = nBlocks + static_cast<int>(cr.channels.size());
    vector<int> parent(nNodes, -1);
    vector<char> seen(nNodes, 0);
    queue<int> q;
    q.push(source);
    seen[source] = 1;

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        if (u == target) break;

        for (const RoutingAdjEdge &e : graph[u]) {
            int v = e.to;
            bool vIsBlock = !isChannelNode(v, nBlocks);

            // Intermediate nodes can only be channels. The only block allowed after source is target.
            if (vIsBlock && v != target) continue;

            if (respectPortEdges) {
                if (u == source && !edgeAllowedByPortConstraint(d.blocks[source], e.edge_from)) continue;
                if (v == target && !edgeAllowedByPortConstraint(d.blocks[target], e.edge_to)) continue;
            }

            if (!seen[v]) {
                seen[v] = 1;
                parent[v] = u;
                q.push(v);
            }
        }
    }

    if (!seen[target]) {
        failReason = respectPortEdges ? "no channel-only path with current PORT EDGE constraints" : "no channel-only path even after relaxing PORT EDGE constraints";
        return false;
    }

    vector<int> rev;
    for (int cur = target; cur != -1; cur = parent[cur]) rev.push_back(cur);
    reverse(rev.begin(), rev.end());
    outNodes = rev;
    return true;
}

static double simpleChannelCapacity(const ChannelRect &ch) {
    // Same conservative capacity model used by the self-checker/report:
    // 25 nets per um times the smaller dimension of the channel rectangle.
    return max(EPS, 25.0 * min(ch.width, ch.height));
}

static double loadPenaltyForEnteringChannel(const ChannelGenerationResult &cr,
                                            const vector<long long> &currentLoads,
                                            int channelNode,
                                            int blockCount,
                                            int nets) {
    int chIdx = channelNode - blockCount;
    if (chIdx < 0 || chIdx >= static_cast<int>(cr.channels.size())) return 0.0;

    double cap = simpleChannelCapacity(cr.channels[chIdx]);
    double before = (chIdx < static_cast<int>(currentLoads.size())) ? static_cast<double>(currentLoads[chIdx]) : 0.0;
    double after = before + static_cast<double>(nets);
    double utilAfter = after / cap;
    double overflowAfter = max(0.0, after - cap) / cap;

    // The first term spreads load before overflow happens. The overflow term is intentionally strong,
    // so a slightly longer path is preferred over pushing another channel far above capacity.
    static constexpr double LOAD_WEIGHT = 20.0;
    static constexpr double OVERFLOW_WEIGHT = 800.0;
    return LOAD_WEIGHT * utilAfter * utilAfter + OVERFLOW_WEIGHT * overflowAfter * overflowAfter;
}


static double softFeedthroughCapacityEstimate(const InputData &d,
                                              const FullPlacementResult &pr,
                                              int blockIdx) {
    if (blockIdx < 0 || blockIdx >= static_cast<int>(d.blocks.size())) return 1.0;
    const FullBlockPlacement &bp = pr.placements[blockIdx];
    double side = min(bp.width, bp.height);
    return max(1000.0, 25.0 * max(1.0, side));
}

static double softFeedthroughPenaltyForEnteringBlock(const InputData &d,
                                                     const FullPlacementResult &pr,
                                                     const vector<long long> &currentFtLoads,
                                                     int blockIdx,
                                                     int nets) {
    if (blockIdx < 0 || blockIdx >= static_cast<int>(d.blocks.size())) return 0.0;
    if (d.blocks[blockIdx].type != "SOFT") return 1e9;
    double cap = softFeedthroughCapacityEstimate(d, pr, blockIdx);
    double before = (blockIdx < static_cast<int>(currentFtLoads.size())) ? static_cast<double>(currentFtLoads[blockIdx]) : 0.0;
    double after = before + static_cast<double>(nets);
    double utilAfter = after / cap;
    double overflowAfter = max(0.0, after - cap) / cap;
    static constexpr double FT_BASE_COST = 0.20;
    static constexpr double FT_LOAD_WEIGHT = 3.0;
    static constexpr double FT_OVERFLOW_WEIGHT = 45.0;
    return FT_BASE_COST + FT_LOAD_WEIGHT * utilAfter * utilAfter + FT_OVERFLOW_WEIGHT * overflowAfter * overflowAfter;
}

static bool isIntermediateSoftBlockAllowed(const InputData &d, int node, int source, int target) {
    if (node < 0 || node >= static_cast<int>(d.blocks.size())) return false;
    if (node == source || node == target) return false;
    return d.blocks[node].type == "SOFT";
}

static bool dijkstraLoadAwareRouteOne(const InputData &d,
                                      const ChannelGenerationResult &cr,
                                      const vector<vector<RoutingAdjEdge>> &graph,
                                      const vector<long long> &currentLoads,
                                      int source,
                                      int target,
                                      int nets,
                                      bool respectPortEdges,
                                      vector<int> &outNodes,
                                      string &failReason) {
    int nBlocks = static_cast<int>(d.blocks.size());
    int nNodes = nBlocks + static_cast<int>(cr.channels.size());
    const double INF = 1e100;
    vector<double> dist(nNodes, INF);
    vector<int> parent(nNodes, -1);

    using State = pair<double, int>;
    priority_queue<State, vector<State>, greater<State>> pq;
    dist[source] = 0.0;
    pq.push({0.0, source});

    while (!pq.empty()) {
        State cur = pq.top();
        double du = cur.first;
        int u = cur.second;
        pq.pop();
        if (du > dist[u] + EPS) continue;
        if (u == target) break;

        for (const RoutingAdjEdge &e : graph[u]) {
            int v = e.to;
            bool vIsBlock = !isChannelNode(v, nBlocks);

            // Channel-only routing: after leaving source, only the target block may be visited.
            if (vIsBlock && v != target) continue;

            if (respectPortEdges) {
                if (u == source && !edgeAllowedByPortConstraint(d.blocks[source], e.edge_from)) continue;
                if (v == target && !edgeAllowedByPortConstraint(d.blocks[target], e.edge_to)) continue;
            }

            double stepCost = 1.0; // hop count keeps paths from taking unnecessary detours.
            if (isChannelNode(v, nBlocks)) {
                stepCost += loadPenaltyForEnteringChannel(cr, currentLoads, v, nBlocks, nets);
            }
            // A tiny deterministic tie-breaker helps keep output stable across platforms.
            stepCost += 1e-6 * static_cast<double>(v + 1);

            if (dist[u] + stepCost + EPS < dist[v]) {
                dist[v] = dist[u] + stepCost;
                parent[v] = u;
                pq.push({dist[v], v});
            }
        }
    }

    if (dist[target] >= INF / 2.0) {
        failReason = respectPortEdges ? "no load-aware channel-only path with current PORT EDGE constraints" : "no load-aware channel-only path even after relaxing PORT EDGE constraints";
        return false;
    }

    vector<int> rev;
    for (int cur = target; cur != -1; cur = parent[cur]) rev.push_back(cur);
    reverse(rev.begin(), rev.end());
    outNodes = rev;
    return true;
}


static bool dijkstraLoadAwareRouteOneWithSoftFeedthrough(const InputData &d,
                                                        const FullPlacementResult &pr,
                                                        const ChannelGenerationResult &cr,
                                                        const vector<vector<RoutingAdjEdge>> &graph,
                                                        const vector<long long> &currentChannelLoads,
                                                        const vector<long long> &currentFtLoads,
                                                        int source,
                                                        int target,
                                                        int nets,
                                                        bool respectPortEdges,
                                                        vector<int> &outNodes,
                                                        string &failReason) {
    int nBlocks = static_cast<int>(d.blocks.size());
    int nNodes = nBlocks + static_cast<int>(cr.channels.size());
    const double INF = 1e100;
    vector<double> dist(nNodes, INF);
    vector<int> parent(nNodes, -1);
    using State = pair<double, int>;
    priority_queue<State, vector<State>, greater<State>> pq;
    dist[source] = 0.0;
    pq.push({0.0, source});
    while (!pq.empty()) {
        State cur = pq.top();
        double du = cur.first;
        int u = cur.second;
        pq.pop();
        if (du > dist[u] + EPS) continue;
        if (u == target) break;
        for (const RoutingAdjEdge &e : graph[u]) {
            int v = e.to;
            bool vIsBlock = !isChannelNode(v, nBlocks);
            if (vIsBlock && v != target && !isIntermediateSoftBlockAllowed(d, v, source, target)) continue;
            if (respectPortEdges) {
                if (u == source && !edgeAllowedByPortConstraint(d.blocks[source], e.edge_from)) continue;
                if (v == target && !edgeAllowedByPortConstraint(d.blocks[target], e.edge_to)) continue;
            }
            double stepCost = 1.0;
            if (isChannelNode(v, nBlocks)) {
                stepCost += loadPenaltyForEnteringChannel(cr, currentChannelLoads, v, nBlocks, nets);
            } else if (isIntermediateSoftBlockAllowed(d, v, source, target)) {
                stepCost += softFeedthroughPenaltyForEnteringBlock(d, pr, currentFtLoads, v, nets);
            }
            stepCost += 1e-6 * static_cast<double>(v + 1);
            if (dist[u] + stepCost + EPS < dist[v]) {
                dist[v] = dist[u] + stepCost;
                parent[v] = u;
                pq.push({dist[v], v});
            }
        }
    }
    if (dist[target] >= INF / 2.0) {
        failReason = respectPortEdges ? "no soft-feedthrough path with current PORT EDGE constraints" : "no soft-feedthrough path even after relaxing PORT EDGE constraints";
        return false;
    }
    vector<int> rev;
    for (int cur = target; cur != -1; cur = parent[cur]) rev.push_back(cur);
    reverse(rev.begin(), rev.end());
    outNodes = rev;
    return true;
}

static bool makeSelfRouteViaOneAdjacentChannel(const InputData &d,
                                               const ChannelGenerationResult &cr,
                                               const vector<vector<RoutingAdjEdge>> &graph,
                                               int block,
                                               bool respectPortEdges,
                                               vector<int> &outNodes,
                                               string &failReason) {
    int nBlocks = static_cast<int>(d.blocks.size());
    for (const RoutingAdjEdge &e : graph[block]) {
        if (!isChannelNode(e.to, nBlocks)) continue;
        if (respectPortEdges && !edgeAllowedByPortConstraint(d.blocks[block], e.edge_from)) continue;
        outNodes = {block, e.to, block};
        return true;
    }
    failReason = respectPortEdges ? "self-connection has no adjacent channel on allowed PORT EDGE" : "self-connection has no adjacent channel";
    (void)cr;
    return false;
}

static bool makeSelfRouteViaLeastLoadedAdjacentChannel(const InputData &d,
                                                       const ChannelGenerationResult &cr,
                                                       const vector<vector<RoutingAdjEdge>> &graph,
                                                       const vector<long long> &currentLoads,
                                                       int block,
                                                       int nets,
                                                       bool respectPortEdges,
                                                       vector<int> &outNodes,
                                                       string &failReason) {
    int nBlocks = static_cast<int>(d.blocks.size());
    double bestCost = 1e100;
    int bestChannel = -1;

    for (const RoutingAdjEdge &e : graph[block]) {
        if (!isChannelNode(e.to, nBlocks)) continue;
        if (respectPortEdges && !edgeAllowedByPortConstraint(d.blocks[block], e.edge_from)) continue;
        double cost = loadPenaltyForEnteringChannel(cr, currentLoads, e.to, nBlocks, nets) + 1e-6 * static_cast<double>(e.to + 1);
        if (cost + EPS < bestCost) {
            bestCost = cost;
            bestChannel = e.to;
        }
    }

    if (bestChannel >= 0) {
        outNodes = {block, bestChannel, block};
        return true;
    }

    // Keep the old simple self-route as an emergency fallback and to preserve behavior for unusual cases.
    return makeSelfRouteViaOneAdjacentChannel(d, cr, graph, block, respectPortEdges, outNodes, failReason);
}


static double estimateRouteWirelengthAndSteps(const InputData &d,
                                               const ChannelGenerationResult &cr,
                                               const vector<vector<RoutingAdjEdge>> &graph,
                                               const vector<int> &nodes,
                                               int nets,
                                               vector<PathStep> &steps) {
    steps.clear();
    vector<pair<double, double>> guidingPoints;
    for (size_t k = 0; k + 1 < nodes.size(); ++k) {
        int u = nodes[k];
        int v = nodes[k + 1];
        const RoutingAdjEdge *e = findGraphEdge(graph, u, v);
        if (!e) continue;
        steps.push_back({nodeName(u, d, cr), e->edge_from});
        steps.push_back({nodeName(v, d, cr), e->edge_to});
        guidingPoints.push_back({e->gx, e->gy});
    }

    double oneNetLength = 0.0;
    for (size_t i = 0; i + 1 < guidingPoints.size(); ++i) {
        oneNetLength += fabs(guidingPoints[i].first - guidingPoints[i + 1].first) +
                        fabs(guidingPoints[i].second - guidingPoints[i + 1].second);
    }
    return oneNetLength * static_cast<double>(nets);
}

static RoutingGenerationResult routeAllConnectionsBaselineBfsChannelOnly(const InputData &d,
                                                                          const FullPlacementResult &pr,
                                                                          const ChannelGenerationResult &cr) {
    RoutingGenerationResult rr;
    rr.channel_loads.assign(cr.channels.size(), 0);
    rr.soft_ft_loads.assign(d.blocks.size(), 0);
    vector<vector<RoutingAdjEdge>> graph = buildRoutingGraph(d, pr, cr);

    int nBlocks = static_cast<int>(d.blocks.size());
    for (const Connection &conn : d.connections) {
        RoutePattern rp;
        rp.from = conn.from;
        rp.to = conn.to;
        rp.nets = conn.nets;

        vector<int> nodes;
        string reason;
        bool ok = false;
        if (conn.from == conn.to) {
            ok = makeSelfRouteViaOneAdjacentChannel(d, cr, graph, conn.from, true, nodes, reason);
        } else {
            ok = bfsRouteOne(d, cr, graph, conn.from, conn.to, true, nodes, reason);
        }
        if (!ok) {
            string strictReason = reason;
            if (conn.from == conn.to) {
                ok = makeSelfRouteViaOneAdjacentChannel(d, cr, graph, conn.from, false, nodes, reason);
            } else {
                ok = bfsRouteOne(d, cr, graph, conn.from, conn.to, false, nodes, reason);
            }
            if (ok) {
                rp.used_port_relaxation = true;
                rr.port_relaxed_count++;
                rr.warnings.push_back(d.blocks[conn.from].name + " -> " + d.blocks[conn.to].name +
                                      " required PORT EDGE relaxation. Strict failure reason: " + strictReason);
            }
        }

        if (!ok) {
            rp.routed = false;
            rp.fail_reason = reason;
            rr.unrouted_count++;
            rr.unrouted_nets += conn.nets;
            rr.errors.push_back("Unrouted connection " + d.blocks[conn.from].name + " -> " + d.blocks[conn.to].name +
                                " nets=" + to_string(conn.nets) + ": " + reason);
        } else {
            rp.routed = true;
            rp.nodes = nodes;
            rp.estimated_wirelength = estimateRouteWirelengthAndSteps(d, cr, graph, nodes, conn.nets, rp.steps);
            rr.routed_count++;
            rr.routed_nets += conn.nets;
            rr.total_estimated_wirelength += rp.estimated_wirelength;
            for (int node : nodes) {
                if (isChannelNode(node, nBlocks)) {
                    int chIdx = node - nBlocks;
                    if (chIdx >= 0 && chIdx < static_cast<int>(rr.channel_loads.size())) rr.channel_loads[chIdx] += conn.nets;
                }
            }
            for (size_t ni = 1; ni + 1 < nodes.size(); ++ni) {
                int node = nodes[ni];
                if (!isChannelNode(node, nBlocks) && node >= 0 && node < nBlocks && d.blocks[node].type == "SOFT") {
                    if (node < static_cast<int>(rr.soft_ft_loads.size())) rr.soft_ft_loads[node] += conn.nets;
                }
            }
        }
        rr.routes.push_back(rp);
    }
    return rr;
}

struct RoutingQualitySummary {
    int overflow_count = 0;
    double total_overflow = 0.0;
    double max_overflow = 0.0;
    double total_wirelength = 0.0;
    int unrouted_count = 0;
};

static RoutingQualitySummary summarizeRoutingQuality(const ChannelGenerationResult &cr,
                                                     const RoutingGenerationResult &rr) {
    RoutingQualitySummary q;
    q.total_wirelength = rr.total_estimated_wirelength;
    q.unrouted_count = rr.unrouted_count;
    for (size_t i = 0; i < cr.channels.size(); ++i) {
        double load = (i < rr.channel_loads.size()) ? static_cast<double>(rr.channel_loads[i]) : 0.0;
        double overflow = max(0.0, load - simpleChannelCapacity(cr.channels[i]));
        if (overflow > EPS) {
            q.overflow_count++;
            q.total_overflow += overflow;
            q.max_overflow = max(q.max_overflow, overflow);
        }
    }
    return q;
}

static string routingQualityToString(const RoutingQualitySummary &q) {
    ostringstream oss;
    oss << "overflow_count=" << q.overflow_count
        << " total_overflow=" << fmt(q.total_overflow)
        << " max_overflow=" << fmt(q.max_overflow)
        << " unrouted=" << q.unrouted_count
        << " wirelength=" << fmt(q.total_wirelength);
    return oss.str();
}

static bool routingQualityBetter(const RoutingQualitySummary &a, const RoutingQualitySummary &b) {
    if (a.unrouted_count != b.unrouted_count) return a.unrouted_count < b.unrouted_count;
    if (fabs(a.total_overflow - b.total_overflow) > 1e-3) return a.total_overflow < b.total_overflow;
    if (a.overflow_count != b.overflow_count) return a.overflow_count < b.overflow_count;
    if (fabs(a.max_overflow - b.max_overflow) > 1e-3) return a.max_overflow < b.max_overflow;
    return a.total_wirelength < b.total_wirelength;
}

static string pairKey(int a, int b);

static vector<Connection> splitConnectionsByTargetChunk(const InputData &d,
                                                        int targetChunk,
                                                        int minSplitNets,
                                                        int maxPartsPerConnection) {
    vector<Connection> jobs;
    for (const Connection &c : d.connections) {
        if (c.nets <= minSplitNets || targetChunk <= 0) {
            jobs.push_back(c);
            continue;
        }
        int parts = static_cast<int>(ceil(static_cast<double>(c.nets) / static_cast<double>(targetChunk)));
        parts = max(1, min(parts, maxPartsPerConnection));
        if (parts <= 1) {
            jobs.push_back(c);
            continue;
        }
        int base = c.nets / parts;
        int rem = c.nets % parts;
        for (int k = 0; k < parts; ++k) {
            Connection piece;
            piece.from = c.from;
            piece.to = c.to;
            piece.nets = base + (k < rem ? 1 : 0);
            if (piece.nets > 0) jobs.push_back(piece);
        }
    }
    return jobs;
}

static string splitJobSummary(const InputData &d, const vector<Connection> &jobs) {
    int splitPairs = 0;
    map<string, int> cnt;
    for (const Connection &j : jobs) cnt[pairKey(j.from, j.to)]++;
    for (const Connection &c : d.connections) if (cnt[pairKey(c.from, c.to)] > 1) splitPairs++;
    stringstream ss;
    ss << "original_connections=" << d.connections.size()
       << " route_jobs=" << jobs.size()
       << " split_connections=" << splitPairs;
    return ss.str();
}

static RoutingGenerationResult routeConnectionJobsLoadAwareChannelOnly(const InputData &d,
                                                                       const FullPlacementResult &pr,
                                                                       const ChannelGenerationResult &cr,
                                                                       const vector<Connection> &jobs,
                                                                       const string &modeLabel) {
    RoutingGenerationResult rr;
    rr.channel_loads.assign(cr.channels.size(), 0);
    rr.soft_ft_loads.assign(d.blocks.size(), 0);
    vector<vector<RoutingAdjEdge>> graph = buildRoutingGraph(d, pr, cr);

    // Route high-demand jobs first. When a large matrix entry is split, each piece can choose
    // a different route, so congestion can be spread across alternate channels.
    vector<int> order(jobs.size());
    iota(order.begin(), order.end(), 0);
    sort(order.begin(), order.end(), [&](int a, int b) {
        const Connection &ca = jobs[a];
        const Connection &cb = jobs[b];
        if (ca.nets != cb.nets) return ca.nets > cb.nets;
        string aa = d.blocks[ca.from].name + "->" + d.blocks[ca.to].name;
        string bb = d.blocks[cb.from].name + "->" + d.blocks[cb.to].name;
        if (aa != bb) return aa < bb;
        return a < b;
    });

    int nBlocks = static_cast<int>(d.blocks.size());
    for (int jobIndex : order) {
        const Connection &conn = jobs[jobIndex];
        RoutePattern rp;
        rp.from = conn.from;
        rp.to = conn.to;
        rp.nets = conn.nets;

        vector<int> nodes;
        string reason;
        bool ok = false;
        if (conn.from == conn.to) {
            ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, rr.channel_loads, conn.from, conn.nets, true, nodes, reason);
        } else {
            ok = dijkstraLoadAwareRouteOne(d, cr, graph, rr.channel_loads, conn.from, conn.to, conn.nets, true, nodes, reason);
        }
        if (!ok) {
            // Try a relaxed-port load-aware route first. If even that fails, fall back to BFS so
            // the solution tries to remain closed instead of creating routing-open failures.
            string strictReason = reason;
            if (conn.from == conn.to) {
                ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, rr.channel_loads, conn.from, conn.nets, false, nodes, reason);
            } else {
                ok = dijkstraLoadAwareRouteOne(d, cr, graph, rr.channel_loads, conn.from, conn.to, conn.nets, false, nodes, reason);
            }
            if (!ok && conn.from != conn.to) {
                ok = bfsRouteOne(d, cr, graph, conn.from, conn.to, false, nodes, reason);
            }
            if (ok) {
                rp.used_port_relaxation = true;
                rr.port_relaxed_count++;
                rr.warnings.push_back(d.blocks[conn.from].name + " -> " + d.blocks[conn.to].name +
                                      " required PORT EDGE relaxation. Strict failure reason: " + strictReason);
            }
        }

        if (!ok) {
            rp.routed = false;
            rp.fail_reason = reason;
            rr.unrouted_count++;
            rr.unrouted_nets += conn.nets;
            rr.errors.push_back("Unrouted route job " + d.blocks[conn.from].name + " -> " + d.blocks[conn.to].name +
                                " nets=" + to_string(conn.nets) + ": " + reason);
        } else {
            rp.routed = true;
            rp.nodes = nodes;
            rp.estimated_wirelength = estimateRouteWirelengthAndSteps(d, cr, graph, nodes, conn.nets, rp.steps);
            rr.routed_count++;
            rr.routed_nets += conn.nets;
            rr.total_estimated_wirelength += rp.estimated_wirelength;
            for (int node : nodes) {
                if (isChannelNode(node, nBlocks)) {
                    int chIdx = node - nBlocks;
                    if (chIdx >= 0 && chIdx < static_cast<int>(rr.channel_loads.size())) rr.channel_loads[chIdx] += conn.nets;
                }
            }
            for (size_t ni = 1; ni + 1 < nodes.size(); ++ni) {
                int node = nodes[ni];
                if (!isChannelNode(node, nBlocks) && node >= 0 && node < nBlocks && d.blocks[node].type == "SOFT") {
                    if (node < static_cast<int>(rr.soft_ft_loads.size())) rr.soft_ft_loads[node] += conn.nets;
                }
            }
        }
        rr.routes.push_back(rp);
    }

    rr.warnings.insert(rr.warnings.begin(), modeLabel + ": " + splitJobSummary(d, jobs));
    return rr;
}

static RoutingGenerationResult routeAllConnectionsLoadAwareChannelOnly(const InputData &d,
                                                                       const FullPlacementResult &pr,
                                                                       const ChannelGenerationResult &cr) {
    return routeConnectionJobsLoadAwareChannelOnly(d, pr, cr, d.connections, "LOAD_AWARE_NO_SPLIT");
}

static RoutingGenerationResult routeAllConnectionsSplitLoadAwareChannelOnly(const InputData &d,
                                                                            const FullPlacementResult &pr,
                                                                            const ChannelGenerationResult &cr,
                                                                            int targetChunk,
                                                                            int minSplitNets,
                                                                            int maxPartsPerConnection) {
    vector<Connection> jobs = splitConnectionsByTargetChunk(d, targetChunk, minSplitNets, maxPartsPerConnection);
    RoutingGenerationResult rr = routeConnectionJobsLoadAwareChannelOnly(
        d, pr, cr, jobs,
        "SPLIT_LOAD_AWARE target_chunk=" + to_string(targetChunk) +
        " min_split_nets=" + to_string(minSplitNets) +
        " max_parts=" + to_string(maxPartsPerConnection));
    return rr;
}


static void resetRoutingAggregateStats(RoutingGenerationResult &rr, size_t channelCount) {
    rr.routed_nets = 0;
    rr.unrouted_nets = 0;
    rr.routed_count = 0;
    rr.unrouted_count = 0;
    rr.port_relaxed_count = 0;
    rr.total_estimated_wirelength = 0.0;
    rr.channel_loads.assign(channelCount, 0);
    if (!rr.soft_ft_loads.empty()) rr.soft_ft_loads.assign(rr.soft_ft_loads.size(), 0);
    rr.errors.clear();
}

static void recomputeRoutingAggregateStats(const InputData &d,
                                           const ChannelGenerationResult &cr,
                                           RoutingGenerationResult &rr) {
    resetRoutingAggregateStats(rr, cr.channels.size());
    rr.soft_ft_loads.assign(d.blocks.size(), 0);
    int nBlocks = static_cast<int>(d.blocks.size());
    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) {
            rr.unrouted_count++;
            rr.unrouted_nets += rp.nets;
            rr.errors.push_back("Unrouted connection " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name +
                                " nets=" + to_string(rp.nets) + ": " + rp.fail_reason);
            continue;
        }
        rr.routed_count++;
        rr.routed_nets += rp.nets;
        if (rp.used_port_relaxation) rr.port_relaxed_count++;
        rr.total_estimated_wirelength += rp.estimated_wirelength;
        for (int node : rp.nodes) {
            if (!isChannelNode(node, nBlocks)) continue;
            int chIdx = node - nBlocks;
            if (chIdx >= 0 && chIdx < static_cast<int>(rr.channel_loads.size())) {
                rr.channel_loads[chIdx] += rp.nets;
            }
        }
        for (size_t ni = 1; ni + 1 < rp.nodes.size(); ++ni) {
            int node = rp.nodes[ni];
            if (!isChannelNode(node, nBlocks) && node >= 0 && node < nBlocks && d.blocks[node].type == "SOFT") {
                if (node < static_cast<int>(rr.soft_ft_loads.size())) rr.soft_ft_loads[node] += rp.nets;
            }
        }
    }
}

static vector<double> computeChannelOverflowVector(const ChannelGenerationResult &cr,
                                                   const vector<long long> &loads) {
    vector<double> overflow(cr.channels.size(), 0.0);
    for (size_t i = 0; i < cr.channels.size(); ++i) {
        double load = (i < loads.size()) ? static_cast<double>(loads[i]) : 0.0;
        overflow[i] = max(0.0, load - simpleChannelCapacity(cr.channels[i]));
    }
    return overflow;
}

static void addRouteLoadToVector(const InputData &d,
                                 const RoutePattern &rp,
                                 vector<long long> &loads,
                                 long long sign) {
    int nBlocks = static_cast<int>(d.blocks.size());
    if (!rp.routed) return;
    for (int node : rp.nodes) {
        if (!isChannelNode(node, nBlocks)) continue;
        int chIdx = node - nBlocks;
        if (chIdx < 0 || chIdx >= static_cast<int>(loads.size())) continue;
        long long next = loads[chIdx] + sign * static_cast<long long>(rp.nets);
        loads[chIdx] = max(0LL, next);
    }
}

static void addRouteSoftFtLoadToVector(const InputData &d,
                                        const RoutePattern &rp,
                                        vector<long long> &ftLoads,
                                        long long sign) {
    int nBlocks = static_cast<int>(d.blocks.size());
    if (!rp.routed) return;
    if (ftLoads.size() < d.blocks.size()) ftLoads.resize(d.blocks.size(), 0);
    for (size_t ni = 1; ni + 1 < rp.nodes.size(); ++ni) {
        int node = rp.nodes[ni];
        if (isChannelNode(node, nBlocks)) continue;
        if (node < 0 || node >= nBlocks) continue;
        if (d.blocks[node].type != "SOFT") continue;
        long long next = ftLoads[node] + sign * static_cast<long long>(rp.nets);
        ftLoads[node] = max(0LL, next);
    }
}

static bool buildOneLoadAwareRoutePattern(const InputData &d,
                                          const ChannelGenerationResult &cr,
                                          const vector<vector<RoutingAdjEdge>> &graph,
                                          const vector<long long> &currentLoads,
                                          int from,
                                          int to,
                                          int nets,
                                          RoutePattern &rp) {
    rp = RoutePattern();
    rp.from = from;
    rp.to = to;
    rp.nets = nets;

    vector<int> nodes;
    string reason;
    bool ok = false;
    if (from == to) {
        ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, currentLoads, from, nets, true, nodes, reason);
    } else {
        ok = dijkstraLoadAwareRouteOne(d, cr, graph, currentLoads, from, to, nets, true, nodes, reason);
    }

    if (!ok) {
        string strictReason = reason;
        if (from == to) {
            ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, currentLoads, from, nets, false, nodes, reason);
        } else {
            ok = dijkstraLoadAwareRouteOne(d, cr, graph, currentLoads, from, to, nets, false, nodes, reason);
        }
        if (!ok && from != to) {
            ok = bfsRouteOne(d, cr, graph, from, to, false, nodes, reason);
        }
        if (ok) {
            rp.used_port_relaxation = true;
            rp.fail_reason = "strict port-edge route was unavailable: " + strictReason;
        }
    }

    if (!ok) {
        rp.routed = false;
        rp.fail_reason = reason;
        return false;
    }

    rp.routed = true;
    rp.nodes = nodes;
    rp.estimated_wirelength = estimateRouteWirelengthAndSteps(d, cr, graph, nodes, nets, rp.steps);
    return true;
}

static bool buildOneSoftFeedthroughRoutePattern(const InputData &d,
                                                   const FullPlacementResult &pr,
                                                   const ChannelGenerationResult &cr,
                                                   const vector<vector<RoutingAdjEdge>> &graph,
                                                   const vector<long long> &currentChannelLoads,
                                                   const vector<long long> &currentFtLoads,
                                                   int from,
                                                   int to,
                                                   int nets,
                                                   RoutePattern &rp) {
    rp = RoutePattern();
    rp.from = from;
    rp.to = to;
    rp.nets = nets;
    vector<int> nodes;
    string reason;
    bool ok = false;
    if (from == to) ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, currentChannelLoads, from, nets, true, nodes, reason);
    else ok = dijkstraLoadAwareRouteOneWithSoftFeedthrough(d, pr, cr, graph, currentChannelLoads, currentFtLoads, from, to, nets, true, nodes, reason);
    if (!ok) {
        string strictReason = reason;
        if (from == to) ok = makeSelfRouteViaLeastLoadedAdjacentChannel(d, cr, graph, currentChannelLoads, from, nets, false, nodes, reason);
        else ok = dijkstraLoadAwareRouteOneWithSoftFeedthrough(d, pr, cr, graph, currentChannelLoads, currentFtLoads, from, to, nets, false, nodes, reason);
        if (!ok && from != to) ok = dijkstraLoadAwareRouteOne(d, cr, graph, currentChannelLoads, from, to, nets, false, nodes, reason);
        if (!ok && from != to) ok = bfsRouteOne(d, cr, graph, from, to, false, nodes, reason);
        if (ok) { rp.used_port_relaxation = true; rp.fail_reason = "strict soft-feedthrough route was unavailable: " + strictReason; }
    }
    if (!ok) { rp.routed = false; rp.fail_reason = reason; return false; }
    rp.routed = true;
    rp.nodes = nodes;
    rp.estimated_wirelength = estimateRouteWirelengthAndSteps(d, cr, graph, nodes, nets, rp.steps);
    return true;
}

static double routeOverflowScore(const InputData &d,
                                 const RoutePattern &rp,
                                 const vector<double> &overflowByChannel) {
    int nBlocks = static_cast<int>(d.blocks.size());
    double score = 0.0;
    set<int> used;
    for (int node : rp.nodes) {
        if (!isChannelNode(node, nBlocks)) continue;
        int chIdx = node - nBlocks;
        if (chIdx < 0 || chIdx >= static_cast<int>(overflowByChannel.size())) continue;
        if (used.insert(chIdx).second) {
            score += overflowByChannel[chIdx];
        }
    }
    if (score > EPS) score *= max(1, rp.nets);
    return score;
}

static RoutingGenerationResult ripUpAndRerouteOverflowChannels(const InputData &d,
                                                               const FullPlacementResult &pr,
                                                               const ChannelGenerationResult &cr,
                                                               const RoutingGenerationResult &start) {
    RoutingGenerationResult best = start;
    recomputeRoutingAggregateStats(d, cr, best);

    vector<vector<RoutingAdjEdge>> graph = buildRoutingGraph(d, pr, cr);
    RoutingQualitySummary bestQ = summarizeRoutingQuality(cr, best);

    static constexpr int MAX_RIPUP_ITER = 10;
    for (int iter = 1; iter <= MAX_RIPUP_ITER; ++iter) {
        vector<double> overflowByChannel = computeChannelOverflowVector(cr, best.channel_loads);
        vector<pair<double, int>> candidates;
        for (int i = 0; i < static_cast<int>(best.routes.size()); ++i) {
            const RoutePattern &rp = best.routes[i];
            if (!rp.routed) continue;
            double score = routeOverflowScore(d, rp, overflowByChannel);
            if (score > EPS) candidates.push_back({score, i});
        }
        if (candidates.empty()) break;

        sort(candidates.begin(), candidates.end(), [](const pair<double, int> &a, const pair<double, int> &b) {
            if (fabs(a.first - b.first) > EPS) return a.first > b.first;
            return a.second < b.second;
        });

        // Reroute only the strongest contributors each iteration. Removing too many at once can
        // destabilize the solution and make paths chase each other around the floorplan.
        int ripCount = min<int>(static_cast<int>(candidates.size()), max(4, static_cast<int>(best.routes.size()) / 5));
        ripCount = min(ripCount, 24);

        RoutingGenerationResult trial = best;
        vector<int> ripped;
        ripped.reserve(ripCount);
        for (int k = 0; k < ripCount; ++k) ripped.push_back(candidates[k].second);

        // Remove all selected route loads first, then reroute them in descending net-count order.
        vector<long long> trialLoads = trial.channel_loads;
        for (int idx : ripped) addRouteLoadToVector(d, trial.routes[idx], trialLoads, -1);
        sort(ripped.begin(), ripped.end(), [&](int a, int b) {
            const RoutePattern &ra = trial.routes[a];
            const RoutePattern &rb = trial.routes[b];
            if (ra.nets != rb.nets) return ra.nets > rb.nets;
            return a < b;
        });

        int changedRoutes = 0;
        for (int idx : ripped) {
            const RoutePattern oldRoute = trial.routes[idx];
            RoutePattern newRoute;
            bool ok = buildOneLoadAwareRoutePattern(d, cr, graph, trialLoads,
                                                    oldRoute.from, oldRoute.to, oldRoute.nets, newRoute);
            if (!ok) {
                // Preserve route closure. If the new attempt fails, put the old route back.
                trial.routes[idx] = oldRoute;
                addRouteLoadToVector(d, oldRoute, trialLoads, +1);
                continue;
            }

            trial.routes[idx] = newRoute;
            addRouteLoadToVector(d, newRoute, trialLoads, +1);
            if (newRoute.nodes != oldRoute.nodes) changedRoutes++;
        }

        recomputeRoutingAggregateStats(d, cr, trial);
        RoutingQualitySummary trialQ = summarizeRoutingQuality(cr, trial);
        if (routingQualityBetter(trialQ, bestQ)) {
            best = trial;
            bestQ = trialQ;
            best.warnings.push_back("RIPUP_REROUTE_ACCEPTED_ITER_" + to_string(iter) +
                                    ": ripped=" + to_string(ripCount) +
                                    " changed=" + to_string(changedRoutes) +
                                    " quality{" + routingQualityToString(bestQ) + "}");
        } else {
            best.warnings.push_back("RIPUP_REROUTE_STOPPED_ITER_" + to_string(iter) +
                                    ": no quality improvement after ripping " + to_string(ripCount) + " routes" +
                                    " | trial{" + routingQualityToString(trialQ) + "}" +
                                    " | best{" + routingQualityToString(bestQ) + "}");
            break;
        }
    }

    best.warnings.insert(best.warnings.begin(), "RIPUP_REROUTE_FINAL: " + routingQualityToString(bestQ));
    return best;
}

static RoutingGenerationResult routeConnectionJobsLoadAwareWithSoftFeedthrough(const InputData &d,
                                                                                  const FullPlacementResult &pr,
                                                                                  const ChannelGenerationResult &cr,
                                                                                  const vector<Connection> &jobs,
                                                                                  const string &modeLabel) {
    RoutingGenerationResult rr;
    rr.channel_loads.assign(cr.channels.size(), 0);
    rr.soft_ft_loads.assign(d.blocks.size(), 0);
    vector<vector<RoutingAdjEdge>> graph = buildRoutingGraph(d, pr, cr);
    vector<int> order(jobs.size());
    iota(order.begin(), order.end(), 0);
    sort(order.begin(), order.end(), [&](int a, int b) {
        const Connection &ca = jobs[a]; const Connection &cb = jobs[b];
        if (ca.nets != cb.nets) return ca.nets > cb.nets;
        string aa = d.blocks[ca.from].name + "->" + d.blocks[ca.to].name;
        string bb = d.blocks[cb.from].name + "->" + d.blocks[cb.to].name;
        if (aa != bb) return aa < bb;
        return a < b;
    });
    int nBlocks = static_cast<int>(d.blocks.size());
    for (int jobIndex : order) {
        const Connection &conn = jobs[jobIndex];
        RoutePattern rp;
        bool ok = buildOneSoftFeedthroughRoutePattern(d, pr, cr, graph, rr.channel_loads, rr.soft_ft_loads, conn.from, conn.to, conn.nets, rp);
        if (!ok) {
            rr.unrouted_count++; rr.unrouted_nets += conn.nets;
            rr.errors.push_back("Unrouted soft-feedthrough route job " + d.blocks[conn.from].name + " -> " + d.blocks[conn.to].name + " nets=" + to_string(conn.nets) + ": " + rp.fail_reason);
        } else {
            rr.routed_count++; rr.routed_nets += conn.nets;
            if (rp.used_port_relaxation) rr.port_relaxed_count++;
            rr.total_estimated_wirelength += rp.estimated_wirelength;
            for (int node : rp.nodes) if (isChannelNode(node, nBlocks)) { int chIdx = node - nBlocks; if (chIdx >= 0 && chIdx < static_cast<int>(rr.channel_loads.size())) rr.channel_loads[chIdx] += conn.nets; }
            for (size_t ni = 1; ni + 1 < rp.nodes.size(); ++ni) { int node = rp.nodes[ni]; if (!isChannelNode(node, nBlocks) && node >= 0 && node < nBlocks && d.blocks[node].type == "SOFT") rr.soft_ft_loads[node] += conn.nets; }
        }
        rr.routes.push_back(rp);
    }
    long long totalFt = 0; int softUsed = 0;
    for (long long v : rr.soft_ft_loads) { totalFt += v; if (v > 0) softUsed++; }
    rr.warnings.insert(rr.warnings.begin(), modeLabel + ": " + splitJobSummary(d, jobs) + " soft_ft_blocks_used=" + to_string(softUsed) + " soft_ft_total_nets=" + to_string(totalFt));
    return rr;
}

static RoutingGenerationResult routeAllConnectionsSoftFeedthroughNoSplit(const InputData &d,
                                                                         const FullPlacementResult &pr,
                                                                         const ChannelGenerationResult &cr) {
    return routeConnectionJobsLoadAwareWithSoftFeedthrough(d, pr, cr, d.connections, "SOFT_FEEDTHROUGH_NO_SPLIT");
}

static RoutingGenerationResult routeAllConnectionsSplitSoftFeedthrough(const InputData &d,
                                                                       const FullPlacementResult &pr,
                                                                       const ChannelGenerationResult &cr,
                                                                       int targetChunk,
                                                                       int minSplitNets,
                                                                       int maxPartsPerConnection) {
    vector<Connection> jobs = splitConnectionsByTargetChunk(d, targetChunk, minSplitNets, maxPartsPerConnection);
    return routeConnectionJobsLoadAwareWithSoftFeedthrough(d, pr, cr, jobs,
        "SPLIT_SOFT_FEEDTHROUGH target_chunk=" + to_string(targetChunk) + " min_split_nets=" + to_string(minSplitNets) + " max_parts=" + to_string(maxPartsPerConnection));
}

static RoutingGenerationResult ripUpAndRerouteOverflowChannelsWithSoftFeedthrough(const InputData &d,
                                                                                  const FullPlacementResult &pr,
                                                                                  const ChannelGenerationResult &cr,
                                                                                  const RoutingGenerationResult &start) {
    RoutingGenerationResult best = start;
    recomputeRoutingAggregateStats(d, cr, best);
    vector<vector<RoutingAdjEdge>> graph = buildRoutingGraph(d, pr, cr);
    RoutingQualitySummary bestQ = summarizeRoutingQuality(cr, best);
    static constexpr int MAX_RIPUP_ITER = 8;
    for (int iter = 1; iter <= MAX_RIPUP_ITER; ++iter) {
        vector<double> overflowByChannel = computeChannelOverflowVector(cr, best.channel_loads);
        vector<pair<double, int>> candidates;
        for (int i = 0; i < static_cast<int>(best.routes.size()); ++i) { const RoutePattern &rp = best.routes[i]; if (!rp.routed) continue; double score = routeOverflowScore(d, rp, overflowByChannel); if (score > EPS) candidates.push_back({score, i}); }
        if (candidates.empty()) break;
        sort(candidates.begin(), candidates.end(), [](const pair<double, int> &a, const pair<double, int> &b) { if (fabs(a.first - b.first) > EPS) return a.first > b.first; return a.second < b.second; });
        int ripCount = min<int>(static_cast<int>(candidates.size()), max(4, static_cast<int>(best.routes.size()) / 5)); ripCount = min(ripCount, 24);
        RoutingGenerationResult trial = best;
        vector<int> ripped; for (int k = 0; k < ripCount; ++k) ripped.push_back(candidates[k].second);
        vector<long long> trialChannelLoads = trial.channel_loads; vector<long long> trialFtLoads = trial.soft_ft_loads;
        for (int idx : ripped) { addRouteLoadToVector(d, trial.routes[idx], trialChannelLoads, -1); addRouteSoftFtLoadToVector(d, trial.routes[idx], trialFtLoads, -1); }
        sort(ripped.begin(), ripped.end(), [&](int a, int b) { if (trial.routes[a].nets != trial.routes[b].nets) return trial.routes[a].nets > trial.routes[b].nets; return a < b; });
        int changedRoutes = 0;
        for (int idx : ripped) {
            const RoutePattern oldRoute = trial.routes[idx]; RoutePattern newRoute;
            bool ok = buildOneSoftFeedthroughRoutePattern(d, pr, cr, graph, trialChannelLoads, trialFtLoads, oldRoute.from, oldRoute.to, oldRoute.nets, newRoute);
            if (!ok) { trial.routes[idx] = oldRoute; addRouteLoadToVector(d, oldRoute, trialChannelLoads, +1); addRouteSoftFtLoadToVector(d, oldRoute, trialFtLoads, +1); continue; }
            trial.routes[idx] = newRoute; addRouteLoadToVector(d, newRoute, trialChannelLoads, +1); addRouteSoftFtLoadToVector(d, newRoute, trialFtLoads, +1); if (newRoute.nodes != oldRoute.nodes) changedRoutes++;
        }
        recomputeRoutingAggregateStats(d, cr, trial);
        RoutingQualitySummary trialQ = summarizeRoutingQuality(cr, trial);
        if (routingQualityBetter(trialQ, bestQ)) { best = trial; bestQ = trialQ; best.warnings.push_back("SOFT_FT_RIPUP_ACCEPTED_ITER_" + to_string(iter) + ": ripped=" + to_string(ripCount) + " changed=" + to_string(changedRoutes) + " quality{" + routingQualityToString(bestQ) + "}"); }
        else { best.warnings.push_back("SOFT_FT_RIPUP_STOPPED_ITER_" + to_string(iter) + ": no quality improvement | trial{" + routingQualityToString(trialQ) + "} | best{" + routingQualityToString(bestQ) + "}"); break; }
    }
    best.warnings.insert(best.warnings.begin(), "SOFT_FT_RIPUP_FINAL: " + routingQualityToString(bestQ));
    return best;
}

static RoutingGenerationResult routeAllConnectionsHybridLoadAwareChannelOnly(const InputData &d,
                                                                             const FullPlacementResult &pr,
                                                                             const ChannelGenerationResult &cr) {
    vector<pair<string, RoutingGenerationResult>> candidates;

    RoutingGenerationResult baseline = routeAllConnectionsBaselineBfsChannelOnly(d, pr, cr);
    candidates.push_back({"BASELINE_BFS", baseline});

    RoutingGenerationResult loadAware = routeAllConnectionsLoadAwareChannelOnly(d, pr, cr);
    candidates.push_back({"LOAD_AWARE_NO_SPLIT", loadAware});

    RoutingGenerationResult rerouted = ripUpAndRerouteOverflowChannels(d, pr, cr, loadAware);
    candidates.push_back({"RIPUP_REROUTE_NO_SPLIT", rerouted});

    RoutingGenerationResult softFt = routeAllConnectionsSoftFeedthroughNoSplit(d, pr, cr);
    candidates.push_back({"SOFT_FEEDTHROUGH_NO_SPLIT", softFt});

    RoutingGenerationResult softFtReroute = ripUpAndRerouteOverflowChannelsWithSoftFeedthrough(d, pr, cr, softFt);
    candidates.push_back({"SOFT_FEEDTHROUGH_RIPUP_NO_SPLIT", softFtReroute});

    // Try several split granularities. Smaller chunks give the router more freedom to distribute
    // traffic, but too many chunks can increase wirelength and output size. The selector below keeps
    // only the best result for the current placement.
    struct SplitCfg { int chunk; int minSplit; int maxParts; };
    vector<SplitCfg> splitCfgs = {
        {2000, 2400, 8},
        {1500, 1800, 10},
        {1000, 1200, 12},
        {750, 1000, 14},
        {500, 800, 16}
    };

    for (const SplitCfg &sc : splitCfgs) {
        RoutingGenerationResult split = routeAllConnectionsSplitLoadAwareChannelOnly(d, pr, cr, sc.chunk, sc.minSplit, sc.maxParts);
        candidates.push_back({"SPLIT_LOAD_AWARE_CHUNK_" + to_string(sc.chunk), split});

        RoutingGenerationResult splitReroute = ripUpAndRerouteOverflowChannels(d, pr, cr, split);
        candidates.push_back({"SPLIT_RIPUP_REROUTE_CHUNK_" + to_string(sc.chunk), splitReroute});

        RoutingGenerationResult splitSoft = routeAllConnectionsSplitSoftFeedthrough(d, pr, cr, sc.chunk, sc.minSplit, sc.maxParts);
        candidates.push_back({"SPLIT_SOFT_FEEDTHROUGH_CHUNK_" + to_string(sc.chunk), splitSoft});

        RoutingGenerationResult splitSoftReroute = ripUpAndRerouteOverflowChannelsWithSoftFeedthrough(d, pr, cr, splitSoft);
        candidates.push_back({"SPLIT_SOFT_FEEDTHROUGH_RIPUP_CHUNK_" + to_string(sc.chunk), splitSoftReroute});
    }

    int best = 0;
    vector<RoutingQualitySummary> summaries;
    summaries.reserve(candidates.size());
    for (const auto &cand : candidates) summaries.push_back(summarizeRoutingQuality(cr, cand.second));
    for (int i = 1; i < static_cast<int>(candidates.size()); ++i) {
        if (routingQualityBetter(summaries[i], summaries[best])) best = i;
    }

    RoutingGenerationResult chosen = candidates[best].second;
    stringstream ss;
    ss << "HYBRID_ROUTING_SELECTED: " << candidates[best].first;
    for (size_t i = 0; i < candidates.size(); ++i) {
        ss << " | " << candidates[i].first << "{" << routingQualityToString(summaries[i]) << "}";
    }
    chosen.warnings.insert(chosen.warnings.begin(), ss.str());

    int splitPathCount = max(0, static_cast<int>(chosen.routes.size()) - static_cast<int>(d.connections.size()));
    chosen.warnings.insert(chosen.warnings.begin(),
        "CONNECTION_SPLITTING_SELECTED_EXTRA_PATHS: " + to_string(splitPathCount) +
        " original_connections=" + to_string(d.connections.size()) +
        " path_records=" + to_string(chosen.routes.size()));
    long long selectedFtNets = 0;
    int selectedFtBlocks = 0;
    for (long long v : chosen.soft_ft_loads) { selectedFtNets += v; if (v > 0) selectedFtBlocks++; }
    chosen.warnings.insert(chosen.warnings.begin(),
        "SOFT_FEEDTHROUGH_SELECTED: soft_blocks_used=" + to_string(selectedFtBlocks) +
        " total_ft_nets=" + to_string(selectedFtNets));
    return chosen;
}


struct ChannelRouteContributor {
    string from_name;
    string to_name;
    int nets = 0;
    int channel_occurrences_in_path = 0;
    double estimated_wirelength = 0.0;
};

struct ChannelOverflowDetail {
    string name;
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
    long long load = 0;
    double capacity = 0.0;
    double overflow = 0.0;
    double utilization = 0.0;
    int path_count = 0;
    vector<ChannelRouteContributor> contributors;
};

static bool routeUsesChannelIndex(const RoutePattern &rp, int channelNodeId) {
    for (int node : rp.nodes) {
        if (node == channelNodeId) return true;
    }
    return false;
}

static int countChannelOccurrencesInRoute(const RoutePattern &rp, int blockCount) {
    int cnt = 0;
    for (int node : rp.nodes) if (isChannelNode(node, blockCount)) cnt++;
    return cnt;
}

static string contributorSummary(const vector<ChannelRouteContributor> &contributors, size_t limit = 5) {
    if (contributors.empty()) return "none";
    vector<ChannelRouteContributor> sorted = contributors;
    sort(sorted.begin(), sorted.end(), [](const ChannelRouteContributor &a, const ChannelRouteContributor &b) {
        if (a.nets != b.nets) return a.nets > b.nets;
        return a.from_name + "->" + a.to_name < b.from_name + "->" + b.to_name;
    });
    ostringstream oss;
    size_t n = min(limit, sorted.size());
    for (size_t i = 0; i < n; ++i) {
        if (i) oss << ";";
        oss << sorted[i].from_name << "->" << sorted[i].to_name << ":" << sorted[i].nets;
    }
    if (sorted.size() > limit) oss << ";..." << (sorted.size() - limit) << " more";
    return oss.str();
}

static vector<ChannelOverflowDetail> buildDetailedChannelOverflowReport(const InputData &d,
                                                                         const ChannelGenerationResult &cr,
                                                                         const RoutingGenerationResult &rr) {
    vector<ChannelOverflowDetail> report;
    int blockCount = static_cast<int>(d.blocks.size());
    report.reserve(cr.channels.size());

    for (size_t i = 0; i < cr.channels.size(); ++i) {
        const ChannelRect &ch = cr.channels[i];
        ChannelOverflowDetail info;
        info.name = ch.name;
        info.lx = ch.lx;
        info.ly = ch.ly;
        info.width = ch.width;
        info.height = ch.height;
        info.load = (i < rr.channel_loads.size()) ? rr.channel_loads[i] : 0;
        // Baseline/simple capacity model used by this self-checker:
        // 25 nets / um times the smaller channel dimension.
        // This is intentionally conservative and is not yet the final official density model.
        info.capacity = 25.0 * min(ch.width, ch.height);
        info.overflow = max(0.0, static_cast<double>(info.load) - info.capacity);
        info.utilization = (info.capacity > EPS) ? static_cast<double>(info.load) / info.capacity : 0.0;

        int channelNodeId = blockCount + static_cast<int>(i);
        for (const RoutePattern &rp : rr.routes) {
            if (!rp.routed) continue;
            if (!routeUsesChannelIndex(rp, channelNodeId)) continue;
            ChannelRouteContributor c;
            c.from_name = d.blocks[rp.from].name;
            c.to_name = d.blocks[rp.to].name;
            c.nets = rp.nets;
            c.channel_occurrences_in_path = countChannelOccurrencesInRoute(rp, blockCount);
            c.estimated_wirelength = rp.estimated_wirelength;
            info.contributors.push_back(c);
        }
        info.path_count = static_cast<int>(info.contributors.size());
        report.push_back(info);
    }

    sort(report.begin(), report.end(), [](const ChannelOverflowDetail &a, const ChannelOverflowDetail &b) {
        bool ao = a.overflow > EPS;
        bool bo = b.overflow > EPS;
        if (ao != bo) return ao > bo;
        if (fabs(a.overflow - b.overflow) > EPS) return a.overflow > b.overflow;
        if (fabs(a.utilization - b.utilization) > EPS) return a.utilization > b.utilization;
        if (a.load != b.load) return a.load > b.load;
        return a.name < b.name;
    });
    return report;
}

static int countOverflowChannels(const vector<ChannelOverflowDetail> &report) {
    int cnt = 0;
    for (const ChannelOverflowDetail &r : report) if (r.overflow > EPS) cnt++;
    return cnt;
}

static double totalOverflowAmount(const vector<ChannelOverflowDetail> &report) {
    double sum = 0.0;
    for (const ChannelOverflowDetail &r : report) sum += r.overflow;
    return sum;
}

static bool verifyAllConnectionsRouted(const InputData &d, const RoutingGenerationResult &rr, vector<string> &errors) {
    bool ok = true;
    map<string, long long> routedNetSum;
    map<string, int> routedPathCount;

    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) {
            errors.push_back("Missing route for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name +
                             " piece_nets=" + to_string(rp.nets));
            ok = false;
            continue;
        }
        string key = pairKey(rp.from, rp.to);
        routedNetSum[key] += rp.nets;
        routedPathCount[key]++;
    }

    for (const Connection &c : d.connections) {
        string key = pairKey(c.from, c.to);
        long long routed = routedNetSum[key];
        if (routed == 0) {
            errors.push_back("Missing PATH for input connection: " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name);
            ok = false;
        } else if (routed != c.nets) {
            errors.push_back("Routed net sum does not match input connection for " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name +
                             ": routed_sum=" + to_string(routed) + " expected=" + to_string(c.nets));
            ok = false;
        }
    }

    set<string> expectedKeys;
    for (const Connection &c : d.connections) expectedKeys.insert(pairKey(c.from, c.to));
    for (const auto &kv : routedNetSum) {
        if (!expectedKeys.count(kv.first)) {
            errors.push_back("Routed unknown/nonzero-matrix-missing pair key=" + kv.first);
            ok = false;
        }
    }
    return ok;
}


static bool verifyRouteIntermediateChannelOrSoftFeedthrough(const InputData &d, const RoutingGenerationResult &rr, vector<string> &errors) {
    int nBlocks = static_cast<int>(d.blocks.size());
    bool ok = true;
    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) continue;
        if (rp.nodes.size() < 3) {
            errors.push_back("Route " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name + " does not pass through any channel or feedthrough block.");
            ok = false;
            continue;
        }
        if (rp.nodes.front() != rp.from || rp.nodes.back() != rp.to) {
            errors.push_back("Route endpoint mismatch for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
            ok = false;
        }
        for (size_t i = 1; i + 1 < rp.nodes.size(); ++i) {
            int node = rp.nodes[i];
            if (isChannelNode(node, nBlocks)) continue;
            if (node >= 0 && node < nBlocks && d.blocks[node].type == "SOFT") continue;
            errors.push_back("Route " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name + " has illegal intermediate node; only CHANNEL or SOFT feedthrough block is allowed.");
            ok = false;
        }
    }
    return ok;
}

static bool verifyPathStepFormat(const InputData &d, const ChannelGenerationResult &cr, const RoutingGenerationResult &rr, vector<string> &errors) {
    bool ok = true;
    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) continue;
        size_t expectedSteps = (rp.nodes.size() - 1) * 2;
        if (rp.steps.size() != expectedSteps) {
            errors.push_back("PATH step count mismatch for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
            ok = false;
            continue;
        }
        if (rp.steps.empty() || rp.steps.front().rect_name != d.blocks[rp.from].name || rp.steps.back().rect_name != d.blocks[rp.to].name) {
            errors.push_back("PATH endpoint name mismatch for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
            ok = false;
        }
        for (const PathStep &s : rp.steps) {
            if (s.edge < 1 || s.edge > 4) {
                errors.push_back("Invalid edge number in PATH for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
                ok = false;
            }
        }
    }
    (void)cr;
    return ok;
}

static bool verifyRoutedNetCount(const InputData &d, const RoutingGenerationResult &rr, vector<string> &errors) {
    long long total = totalDirectedNets(d);
    if (rr.routed_nets + rr.unrouted_nets != total) {
        errors.push_back("Routed nets + unrouted nets does not equal input total directed nets.");
        return false;
    }
    if (rr.unrouted_nets != 0) {
        errors.push_back("There are unrouted nets: " + to_string(rr.unrouted_nets));
        return false;
    }
    return true;
}

static string routePathAsText(const RoutePattern &rp) {
    ostringstream oss;
    oss << "PATH " << rp.nets;
    for (const PathStep &s : rp.steps) oss << " " << s.rect_name << " " << s.edge;
    return oss.str();
}



struct CfgWriteResult {
    bool written = false;
    string output_path;
    int block_count = 0;
    int channel_count = 0;
    int path_count = 0;
    vector<string> warnings;
    vector<string> errors;
};

static string makeCfgOutputPath(const string &inputPath) {
    size_t slash = inputPath.find_last_of("/\\");
    size_t dot = inputPath.find_last_of('.');
    if (dot != string::npos && (slash == string::npos || dot > slash)) {
        return inputPath.substr(0, dot) + ".cfg";
    }
    return inputPath + ".cfg";
}

static CfgWriteResult writeCfgFile(const string &inputPath,
                                   const InputData &d,
                                   const FullPlacementResult &pr,
                                   const ChannelGenerationResult &cr,
                                   const RoutingGenerationResult &rr) {
    CfgWriteResult res;
    res.output_path = makeCfgOutputPath(inputPath);
    res.block_count = static_cast<int>(d.blocks.size());
    res.channel_count = static_cast<int>(cr.channels.size());
    for (const RoutePattern &rp : rr.routes) {
        if (rp.routed) res.path_count++;
    }

    if (!d.errors.empty()) res.errors.push_back("Input parser has errors; cfg not guaranteed valid.");
    if (!pr.errors.empty()) res.errors.push_back("Placement has errors; cfg not guaranteed valid.");
    if (!cr.errors.empty()) res.errors.push_back("Channel generation has errors; cfg not guaranteed valid.");
    if (!rr.errors.empty()) res.errors.push_back("Routing generation has errors; cfg not guaranteed valid.");
    if (rr.unrouted_count != 0) res.errors.push_back("There are unrouted paths; cfg PATH section would be incomplete.");

    ofstream fout(res.output_path);
    if (!fout) {
        res.errors.push_back("Cannot open output cfg file for writing: " + res.output_path);
        return res;
    }

    // Output format follows the statement example:
    // Outline <width> <height>
    // BLOCK / NumBlocks / BLOCK ... / END BLOCK
    // CHANNEL / NumChannels / CHANNEL ... / END CHANNEL
    // PATH / NumRoutingPattern / PATH ... / END PATH
    fout << "Outline " << fmt(d.outline_width) << " " << fmt(d.outline_height) << "\n\n";

    fout << "BLOCK\n";
    fout << "NumBlocks " << d.blocks.size() << "\n";
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        const FullBlockPlacement &bp = pr.placements[i];
        fout << "BLOCK " << d.blocks[i].name << " "
             << fmt(bp.lx) << " " << fmt(bp.ly) << " "
             << fmt(bp.width) << " " << fmt(bp.height) << "\n";
    }
    fout << "END BLOCK\n\n";

    fout << "CHANNEL\n";
    fout << "NumChannels " << cr.channels.size() << "\n";
    for (const ChannelRect &ch : cr.channels) {
        fout << "CHANNEL " << ch.name << " "
             << fmt(ch.lx) << " " << fmt(ch.ly) << " "
             << fmt(ch.width) << " " << fmt(ch.height) << "\n";
    }
    fout << "END CHANNEL\n\n";

    fout << "PATH\n";
    fout << "NumRoutingPattern " << res.path_count << "\n";
    for (const RoutePattern &rp : rr.routes) {
        if (rp.routed) fout << routePathAsText(rp) << "\n";
    }
    fout << "END PATH\n";

    if (!fout.good()) {
        res.errors.push_back("Write error occurred while writing cfg file: " + res.output_path);
        return res;
    }

    res.written = true;
    return res;
}



struct CfgBlockRecord {
    string name;
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
    int line_no = 0;
};

struct CfgChannelRecord {
    string name;
    double lx = 0.0;
    double ly = 0.0;
    double width = 0.0;
    double height = 0.0;
    int line_no = 0;
};

struct CfgPathRecord {
    long long nets = 0;
    vector<PathStep> steps;
    int line_no = 0;
};

struct CfgParsedFile {
    bool file_opened = false;
    bool saw_outline = false;
    bool saw_block_section = false;
    bool saw_channel_section = false;
    bool saw_path_section = false;
    bool block_section_closed = false;
    bool channel_section_closed = false;
    bool path_section_closed = false;
    double outline_width = 0.0;
    double outline_height = 0.0;
    int declared_blocks = -1;
    int declared_channels = -1;
    int declared_paths = -1;
    int num_blocks_line_count = 0;
    int num_channels_line_count = 0;
    int num_paths_line_count = 0;
    vector<string> top_level_order;
    vector<CfgBlockRecord> blocks;
    vector<CfgChannelRecord> channels;
    vector<CfgPathRecord> paths;
};

struct CfgValidationResult {
    string cfg_path;
    CfgParsedFile parsed;
    bool format_ok = false;
    bool geometry_ok = false;
    bool routing_ok = false;
    bool capacity_ok = true; // capacity overflow is only a warning for now
    bool strict_ok = false;
    bool overall_pass = false;
    long long total_routed_nets_in_cfg = 0;
    int overflow_channel_count = 0;
    vector<string> format_errors;
    vector<string> geometry_errors;
    vector<string> routing_errors;
    vector<string> capacity_warnings;
    vector<string> warnings;
};

static vector<string> splitWs(const string &line) {
    vector<string> out;
    istringstream iss(line);
    string tok;
    while (iss >> tok) out.push_back(tok);
    return out;
}

static bool parseDoubleStrict(const string &s, double &v) {
    try {
        size_t pos = 0;
        v = stod(s, &pos);
        return pos == s.size() && isfinite(v);
    } catch (...) {
        return false;
    }
}

static bool parseLongLongStrict(const string &s, long long &v) {
    try {
        size_t pos = 0;
        v = stoll(s, &pos);
        return pos == s.size();
    } catch (...) {
        return false;
    }
}

static bool parseIntStrict(const string &s, int &v) {
    long long tmp = 0;
    if (!parseLongLongStrict(s, tmp)) return false;
    if (tmp < -2147483647LL || tmp > 2147483647LL) return false;
    v = static_cast<int>(tmp);
    return true;
}

static string pairKey(int a, int b) {
    return to_string(a) + "->" + to_string(b);
}

static Rect rectFromCfgBlock(const CfgBlockRecord &b) {
    return Rect{b.name, b.lx, b.ly, b.width, b.height};
}

static Rect rectFromCfgChannel(const CfgChannelRecord &c) {
    return Rect{c.name, c.lx, c.ly, c.width, c.height};
}

static bool cfgRectsOverlap(double ax, double ay, double aw, double ah, double bx, double by, double bw, double bh) {
    const double TOL = 1e-4;
    if (ax + aw <= bx + TOL) return false;
    if (bx + bw <= ax + TOL) return false;
    if (ay + ah <= by + TOL) return false;
    if (by + bh <= ay + TOL) return false;
    return true;
}

static bool cfgInsideOutline(double lx, double ly, double w, double h, double outlineW, double outlineH) {
    const double TOL = 1e-4;
    return lx >= -TOL && ly >= -TOL && lx + w <= outlineW + TOL && ly + h <= outlineH + TOL;
}

static CfgParsedFile parseCfgFileForSelfCheck(const string &cfgPath, vector<string> &formatErrors) {
    CfgParsedFile parsed;
    ifstream fin(cfgPath);
    if (!fin) {
        formatErrors.push_back("Cannot open generated cfg file: " + cfgPath);
        return parsed;
    }
    parsed.file_opened = true;

    enum class Section { NONE, BLOCK, CHANNEL, PATH };
    Section section = Section::NONE;
    string line;
    int lineNo = 0;
    while (getline(fin, line)) {
        ++lineNo;
        string tline = trim(line);
        if (tline.empty()) continue;
        if (!tline.empty() && tline[0] == '#') continue;
        vector<string> tok = splitWs(tline);
        if (tok.empty()) continue;
        string key = upperNoSpace(tok[0]);

        if (section == Section::NONE) {
            if (key == "OUTLINE") {
                if (parsed.saw_outline) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated Outline line.");
                    continue;
                }
                if (tok.size() != 3 || !parseDoubleStrict(tok[1], parsed.outline_width) || !parseDoubleStrict(tok[2], parsed.outline_height)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": Outline must be: Outline <width> <height>.");
                    continue;
                }
                parsed.saw_outline = true;
                parsed.top_level_order.push_back("OUTLINE");
            } else if (key == "BLOCK") {
                if (parsed.saw_block_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated BLOCK section.");
                parsed.saw_block_section = true;
                parsed.top_level_order.push_back("BLOCK");
                section = Section::BLOCK;
            } else if (key == "CHANNEL") {
                if (parsed.saw_channel_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated CHANNEL section.");
                parsed.saw_channel_section = true;
                parsed.top_level_order.push_back("CHANNEL");
                section = Section::CHANNEL;
            } else if (key == "PATH") {
                if (parsed.saw_path_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated PATH section.");
                parsed.saw_path_section = true;
                parsed.top_level_order.push_back("PATH");
                section = Section::PATH;
            } else {
                formatErrors.push_back("Line " + to_string(lineNo) + ": unexpected top-level token: " + tok[0]);
            }
            continue;
        }

        if (section == Section::BLOCK) {
            if (key == "END") {
                if (tok.size() != 2 || upperNoSpace(tok[1]) != "BLOCK") {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": BLOCK section must end with END BLOCK.");
                }
                parsed.block_section_closed = true;
                section = Section::NONE;
            } else if (key == "NUMBLOCKS") {
                parsed.num_blocks_line_count++;
                if (parsed.num_blocks_line_count > 1) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated NumBlocks line.");
                }
                if (!parsed.blocks.empty()) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumBlocks must appear before BLOCK records.");
                }
                if (tok.size() != 2 || !parseIntStrict(tok[1], parsed.declared_blocks)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumBlocks must be followed by one integer.");
                }
            } else if (key == "BLOCK") {
                if (tok.size() != 6) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": BLOCK record must be: BLOCK <name> <lx> <ly> <width> <height>.");
                    continue;
                }
                CfgBlockRecord b;
                b.name = tok[1];
                b.line_no = lineNo;
                if (!parseDoubleStrict(tok[2], b.lx) || !parseDoubleStrict(tok[3], b.ly) ||
                    !parseDoubleStrict(tok[4], b.width) || !parseDoubleStrict(tok[5], b.height)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": BLOCK coordinates/sizes must be numeric.");
                    continue;
                }
                parsed.blocks.push_back(b);
            } else {
                formatErrors.push_back("Line " + to_string(lineNo) + ": unexpected token in BLOCK section: " + tok[0]);
            }
            continue;
        }

        if (section == Section::CHANNEL) {
            if (key == "END") {
                if (tok.size() != 2 || upperNoSpace(tok[1]) != "CHANNEL") {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": CHANNEL section must end with END CHANNEL.");
                }
                parsed.channel_section_closed = true;
                section = Section::NONE;
            } else if (key == "NUMCHANNELS") {
                parsed.num_channels_line_count++;
                if (parsed.num_channels_line_count > 1) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated NumChannels line.");
                }
                if (!parsed.channels.empty()) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumChannels must appear before CHANNEL records.");
                }
                if (tok.size() != 2 || !parseIntStrict(tok[1], parsed.declared_channels)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumChannels must be followed by one integer.");
                }
            } else if (key == "CHANNEL") {
                if (tok.size() != 6) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": CHANNEL record must be: CHANNEL <name> <lx> <ly> <width> <height>.");
                    continue;
                }
                CfgChannelRecord c;
                c.name = tok[1];
                c.line_no = lineNo;
                if (!parseDoubleStrict(tok[2], c.lx) || !parseDoubleStrict(tok[3], c.ly) ||
                    !parseDoubleStrict(tok[4], c.width) || !parseDoubleStrict(tok[5], c.height)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": CHANNEL coordinates/sizes must be numeric.");
                    continue;
                }
                parsed.channels.push_back(c);
            } else {
                formatErrors.push_back("Line " + to_string(lineNo) + ": unexpected token in CHANNEL section: " + tok[0]);
            }
            continue;
        }

        if (section == Section::PATH) {
            if (key == "END") {
                if (tok.size() != 2 || upperNoSpace(tok[1]) != "PATH") {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": PATH section must end with END PATH.");
                }
                parsed.path_section_closed = true;
                section = Section::NONE;
            } else if (key == "NUMROUTINGPATTERN") {
                parsed.num_paths_line_count++;
                if (parsed.num_paths_line_count > 1) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated NumRoutingPattern line.");
                }
                if (!parsed.paths.empty()) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumRoutingPattern must appear before PATH records.");
                }
                if (tok.size() != 2 || !parseIntStrict(tok[1], parsed.declared_paths)) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": NumRoutingPattern must be followed by one integer.");
                }
            } else if (key == "PATH") {
                if (tok.size() < 6 || ((tok.size() - 2) % 2) != 0) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": PATH must be PATH <net_count> followed by rectangle-edge pairs.");
                    continue;
                }
                CfgPathRecord p;
                p.line_no = lineNo;
                if (!parseLongLongStrict(tok[1], p.nets) || p.nets <= 0) {
                    formatErrors.push_back("Line " + to_string(lineNo) + ": PATH net_count must be a positive integer.");
                    continue;
                }
                bool ok = true;
                for (size_t i = 2; i + 1 < tok.size(); i += 2) {
                    int edge = 0;
                    if (!parseIntStrict(tok[i + 1], edge)) {
                        formatErrors.push_back("Line " + to_string(lineNo) + ": PATH edge must be an integer.");
                        ok = false;
                        break;
                    }
                    if (edge < 1 || edge > 4) {
                        formatErrors.push_back("Line " + to_string(lineNo) + ": PATH edge must be 1, 2, 3, or 4.");
                        ok = false;
                        break;
                    }
                    p.steps.push_back(PathStep{tok[i], edge});
                }
                if (ok) parsed.paths.push_back(p);
            } else {
                formatErrors.push_back("Line " + to_string(lineNo) + ": unexpected token in PATH section: " + tok[0]);
            }
            continue;
        }
    }

    if (section == Section::BLOCK) formatErrors.push_back("CFG ended before END BLOCK.");
    if (section == Section::CHANNEL) formatErrors.push_back("CFG ended before END CHANNEL.");
    if (section == Section::PATH) formatErrors.push_back("CFG ended before END PATH.");

    if (!parsed.saw_outline) formatErrors.push_back("Missing Outline line.");
    if (!parsed.saw_block_section) formatErrors.push_back("Missing BLOCK section.");
    if (!parsed.saw_channel_section) formatErrors.push_back("Missing CHANNEL section.");
    if (!parsed.saw_path_section) formatErrors.push_back("Missing PATH section.");
    if (parsed.saw_block_section && !parsed.block_section_closed) formatErrors.push_back("BLOCK section is not closed by END BLOCK.");
    if (parsed.saw_channel_section && !parsed.channel_section_closed) formatErrors.push_back("CHANNEL section is not closed by END CHANNEL.");
    if (parsed.saw_path_section && !parsed.path_section_closed) formatErrors.push_back("PATH section is not closed by END PATH.");

    vector<string> expectedOrder = {"OUTLINE", "BLOCK", "CHANNEL", "PATH"};
    if (parsed.top_level_order != expectedOrder) {
        string got;
        for (size_t i = 0; i < parsed.top_level_order.size(); ++i) {
            if (i) got += " -> ";
            got += parsed.top_level_order[i];
        }
        formatErrors.push_back("Top-level cfg section order must be Outline -> BLOCK -> CHANNEL -> PATH. Got: " + got);
    }

    if (parsed.num_blocks_line_count == 0) formatErrors.push_back("Missing NumBlocks line.");
    else if (parsed.declared_blocks < 0) formatErrors.push_back("NumBlocks value cannot be negative.");
    else if (parsed.declared_blocks != static_cast<int>(parsed.blocks.size())) formatErrors.push_back("NumBlocks does not match actual BLOCK records.");
    if (parsed.num_channels_line_count == 0) formatErrors.push_back("Missing NumChannels line.");
    else if (parsed.declared_channels < 0) formatErrors.push_back("NumChannels value cannot be negative.");
    else if (parsed.declared_channels != static_cast<int>(parsed.channels.size())) formatErrors.push_back("NumChannels does not match actual CHANNEL records.");
    if (parsed.num_paths_line_count == 0) formatErrors.push_back("Missing NumRoutingPattern line.");
    else if (parsed.declared_paths < 0) formatErrors.push_back("NumRoutingPattern value cannot be negative.");
    else if (parsed.declared_paths != static_cast<int>(parsed.paths.size())) formatErrors.push_back("NumRoutingPattern does not match actual PATH records.");

    return parsed;
}

static bool validCfgNameToken(const string &name) {
    if (name.empty()) return false;
    for (char ch : name) {
        unsigned char c = static_cast<unsigned char>(ch);
        if (isspace(c)) return false;
        if (ch == ',' || ch == ';' || ch == '#') return false;
    }
    return true;
}

static CfgValidationResult validateGeneratedCfgFile(const string &cfgPath, const InputData &d) {
    CfgValidationResult vr;
    vr.cfg_path = cfgPath;
    vr.parsed = parseCfgFileForSelfCheck(cfgPath, vr.format_errors);
    const CfgParsedFile &p = vr.parsed;

    if (p.file_opened && p.saw_outline) {
        if (p.outline_width <= EPS || p.outline_height <= EPS) {
            vr.geometry_errors.push_back("CFG outline width/height must be positive.");
        }
        if (p.outline_width > d.outline_width + EPS || p.outline_height > d.outline_height + EPS) {
            vr.geometry_errors.push_back("CFG outline exceeds input OUTLINE MAX.");
        }
    }

    map<string, int> inputBlockIdx;
    for (size_t i = 0; i < d.blocks.size(); ++i) inputBlockIdx[d.blocks[i].name] = static_cast<int>(i);

    map<string, CfgBlockRecord> cfgBlocks;
    map<string, CfgChannelRecord> cfgChannels;
    map<string, int> nameUseCount;

    for (const CfgBlockRecord &b : p.blocks) {
        if (!validCfgNameToken(b.name)) vr.format_errors.push_back("Line " + to_string(b.line_no) + ": invalid BLOCK name token: " + b.name);
        nameUseCount[b.name]++;
        if (cfgBlocks.count(b.name)) {
            vr.geometry_errors.push_back("Duplicated BLOCK name in cfg: " + b.name);
        } else {
            cfgBlocks[b.name] = b;
        }
        if (!inputBlockIdx.count(b.name)) vr.geometry_errors.push_back("CFG contains unknown BLOCK name: " + b.name);
        if (b.width <= EPS || b.height <= EPS) vr.geometry_errors.push_back("BLOCK has non-positive size: " + b.name);
        if (p.saw_outline && !cfgInsideOutline(b.lx, b.ly, b.width, b.height, p.outline_width, p.outline_height)) {
            vr.geometry_errors.push_back("BLOCK outside cfg outline: " + b.name);
        }
    }

    if (p.blocks.size() != d.blocks.size()) {
        vr.geometry_errors.push_back("CFG BLOCK record count does not exactly equal input BLOCK count.");
    }
    for (const Block &bi : d.blocks) {
        if (!cfgBlocks.count(bi.name)) vr.geometry_errors.push_back("Missing BLOCK from cfg: " + bi.name);
    }

    for (size_t i = 0; i < d.blocks.size(); ++i) {
        const Block &bi = d.blocks[i];
        if (!cfgBlocks.count(bi.name)) continue;
        const CfgBlockRecord &b = cfgBlocks[bi.name];
        double aspect = b.height > EPS ? b.width / b.height : 0.0;
        double area = b.width * b.height;
        if (bi.type == "EDGE" || bi.type == "MACRO") {
            if (fabs(b.width - bi.width) > 1e-4 || fabs(b.height - bi.height) > 1e-4) {
                vr.geometry_errors.push_back(bi.name + " fixed EDGE/MACRO size changed in cfg.");
            }
        }
        if (bi.type == "SOFT") {
            if (area + max(1.0, bi.area) * 1e-4 < bi.area) {
                vr.geometry_errors.push_back(bi.name + " SOFT area is smaller than input area in cfg.");
            }
            if (aspect + 1e-5 < bi.ar_min || aspect - 1e-5 > bi.ar_max) {
                vr.geometry_errors.push_back(bi.name + " SOFT aspect ratio outside input range in cfg.");
            }
        }
        if (bi.type == "EDGE") {
            Rect r{bi.name, b.lx, b.ly, b.width, b.height};
            bool satisfiesAny = false;
            for (const string &loc : bi.locations) {
                if (locationSatisfied(r, loc, p.outline_width, p.outline_height)) {
                    satisfiesAny = true;
                    break;
                }
            }
            if (!satisfiesAny) {
                vr.geometry_errors.push_back(bi.name + " does not satisfy any allowed EDGE LOCATION in cfg.");
            }
        }
    }

    for (size_t i = 0; i < p.blocks.size(); ++i) {
        for (size_t j = i + 1; j < p.blocks.size(); ++j) {
            const auto &a = p.blocks[i];
            const auto &b = p.blocks[j];
            if (cfgRectsOverlap(a.lx, a.ly, a.width, a.height, b.lx, b.ly, b.width, b.height)) {
                vr.geometry_errors.push_back("CFG BLOCK overlap: " + a.name + " overlaps " + b.name);
            }
        }
    }

    for (const CfgChannelRecord &c : p.channels) {
        if (!validCfgNameToken(c.name)) vr.format_errors.push_back("Line " + to_string(c.line_no) + ": invalid CHANNEL name token: " + c.name);
        nameUseCount[c.name]++;
        if (cfgChannels.count(c.name)) {
            vr.geometry_errors.push_back("Duplicated CHANNEL name in cfg: " + c.name);
        } else {
            cfgChannels[c.name] = c;
        }
        if (c.name.size() > 30) vr.format_errors.push_back("CHANNEL name longer than 30 characters: " + c.name);
        if (inputBlockIdx.count(c.name)) vr.geometry_errors.push_back("CHANNEL name collides with BLOCK name: " + c.name);
        if (c.width <= EPS || c.height <= EPS) vr.geometry_errors.push_back("CHANNEL has non-positive size: " + c.name);
        if (p.saw_outline && !cfgInsideOutline(c.lx, c.ly, c.width, c.height, p.outline_width, p.outline_height)) {
            vr.geometry_errors.push_back("CHANNEL outside cfg outline: " + c.name);
        }
        for (const CfgBlockRecord &b : p.blocks) {
            if (cfgRectsOverlap(c.lx, c.ly, c.width, c.height, b.lx, b.ly, b.width, b.height)) {
                vr.geometry_errors.push_back("CFG CHANNEL overlaps BLOCK: " + c.name + " overlaps " + b.name);
            }
        }
    }

    for (size_t i = 0; i < p.channels.size(); ++i) {
        for (size_t j = i + 1; j < p.channels.size(); ++j) {
            const auto &a = p.channels[i];
            const auto &b = p.channels[j];
            if (cfgRectsOverlap(a.lx, a.ly, a.width, a.height, b.lx, b.ly, b.width, b.height)) {
                vr.geometry_errors.push_back("CFG CHANNEL overlap: " + a.name + " overlaps " + b.name);
            }
        }
    }

    for (const auto &kv : nameUseCount) {
        if (kv.second > 1) {
            vr.format_errors.push_back("Rectangle name is not globally unique across BLOCK/CHANNEL records: " + kv.first);
        }
    }

    map<string, Rect> rectByName;
    map<string, string> rectKind; // BLOCK or CHANNEL
    map<string, int> rectBlockIndex;
    for (const CfgBlockRecord &b : p.blocks) {
        rectByName[b.name] = rectFromCfgBlock(b);
        rectKind[b.name] = "BLOCK";
        if (inputBlockIdx.count(b.name)) rectBlockIndex[b.name] = inputBlockIdx[b.name];
    }
    for (const CfgChannelRecord &c : p.channels) {
        rectByName[c.name] = rectFromCfgChannel(c);
        rectKind[c.name] = "CHANNEL";
    }

    map<string, int> routedPairCount;
    map<string, long long> routedPairNetSum;
    map<string, long long> channelLoads;
    map<string, long long> softFtLoads;

    for (const CfgPathRecord &path : p.paths) {
        vr.total_routed_nets_in_cfg += path.nets;
        if (path.steps.size() < 4 || (path.steps.size() % 2) != 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH must have an even number of rectangle-edge pairs and at least one intermediate rectangle.");
            continue;
        }

        const string &srcName = path.steps.front().rect_name;
        const string &dstName = path.steps.back().rect_name;
        for (const PathStep &s : path.steps) {
            if (!validCfgNameToken(s.rect_name)) {
                vr.format_errors.push_back("Line " + to_string(path.line_no) + ": invalid rectangle name token in PATH: " + s.rect_name);
            }
        }
        if (!rectByName.count(srcName)) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": source rectangle not defined: " + srcName);
            continue;
        }
        if (!rectByName.count(dstName)) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": destination rectangle not defined: " + dstName);
            continue;
        }
        if (rectKind[srcName] != "BLOCK" || rectKind[dstName] != "BLOCK") {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH first and last rectangles must be BLOCK names.");
            continue;
        }
        int srcIdx = rectBlockIndex.count(srcName) ? rectBlockIndex[srcName] : -1;
        int dstIdx = rectBlockIndex.count(dstName) ? rectBlockIndex[dstName] : -1;
        if (srcIdx < 0 || dstIdx < 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH endpoint block not found in input.");
            continue;
        }
        if (srcIdx == dstIdx) {
            vr.warnings.push_back("Line " + to_string(path.line_no) + ": diagonal/self connection routed for " + srcName + "; accepted because the input matrix contains it.");
        }
        if (!edgeAllowedByPortConstraint(d.blocks[srcIdx], path.steps.front().edge)) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": source edge violates PORT EDGE constraint for " + srcName);
        }
        if (!edgeAllowedByPortConstraint(d.blocks[dstIdx], path.steps.back().edge)) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": destination edge violates PORT EDGE constraint for " + dstName);
        }
        if (path.nets <= 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH net_count must be positive.");
        }
        if (d.conn[srcIdx][dstIdx] <= 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH endpoint pair is not a nonzero input connection: " + srcName + " -> " + dstName);
        }
        string routedKey = pairKey(srcIdx, dstIdx);
        routedPairCount[routedKey]++;
        routedPairNetSum[routedKey] += path.nets;

        for (const PathStep &s : path.steps) {
            if (!rectByName.count(s.rect_name)) {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH uses undefined rectangle: " + s.rect_name);
            }
            if (s.edge < 1 || s.edge > 4) {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": invalid edge number for rectangle " + s.rect_name);
            }
        }

        for (size_t i = 1; i + 1 < path.steps.size(); i += 2) {
            if (path.steps[i].rect_name != path.steps[i + 1].rect_name) {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": intermediate rectangle is not one-in-one-out near " + path.steps[i].rect_name + ".");
            } else {
                const string &midName = path.steps[i].rect_name;
                if (rectKind.count(midName) && rectKind[midName] != "CHANNEL") {
                    bool okSoftFt = false;
                    if (rectKind[midName] == "BLOCK" && rectBlockIndex.count(midName)) {
                        int midIdx = rectBlockIndex[midName];
                        okSoftFt = (d.blocks[midIdx].type == "SOFT");
                    }
                    if (!okSoftFt) {
                        vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": paired intermediate rectangle is neither CHANNEL nor SOFT feedthrough block: " + midName);
                    }
                }
                if (path.steps[i].edge == path.steps[i + 1].edge) {
                    vr.warnings.push_back("Line " + to_string(path.line_no) + ": intermediate rectangle enters and exits on the same edge: " + midName + "; accepted but marked as a routing-quality warning.");
                }
            }
        }

        map<string, bool> touchedChannels;
        map<string, int> touchedSoftFtCount;
        for (size_t i = 1; i + 1 < path.steps.size(); ++i) {
            const string &nm = path.steps[i].rect_name;
            if (!rectKind.count(nm)) continue;
            if (rectKind[nm] == "CHANNEL") {
                touchedChannels[nm] = true;
            } else {
                bool okSoftFt = false;
                if (rectKind[nm] == "BLOCK" && rectBlockIndex.count(nm)) {
                    int midIdx = rectBlockIndex[nm];
                    okSoftFt = (d.blocks[midIdx].type == "SOFT" && nm != srcName && nm != dstName);
                }
                if (!okSoftFt) {
                    vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": intermediate rectangle is neither CHANNEL nor SOFT feedthrough block: " + nm);
                } else {
                    touchedSoftFtCount[nm]++;
                }
            }
        }
        for (const auto &kv : touchedChannels) channelLoads[kv.first] += path.nets;
        for (const auto &kv : touchedSoftFtCount) {
            int occurrences = max(1, kv.second / 2);
            softFtLoads[kv.first] += static_cast<long long>(path.nets) * occurrences;
        }

        for (size_t i = 0; i + 1 < path.steps.size(); i += 2) {
            const PathStep &aStep = path.steps[i];
            const PathStep &bStep = path.steps[i + 1];
            if (!rectByName.count(aStep.rect_name) || !rectByName.count(bStep.rect_name)) continue;
            int expectedA = 0, expectedB = 0;
            double gx = 0.0, gy = 0.0;
            bool adjacent = adjacentRectsForRouting(rectByName[aStep.rect_name], rectByName[bStep.rect_name], expectedA, expectedB, gx, gy);
            if (!adjacent) {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": consecutive rectangles are not geometrically adjacent: " + aStep.rect_name + " -> " + bStep.rect_name);
            } else if (expectedA != aStep.edge || expectedB != bStep.edge) {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": edge mismatch for contact " + aStep.rect_name + " -> " + bStep.rect_name +
                                            ", expected " + to_string(expectedA) + "/" + to_string(expectedB) +
                                            ", got " + to_string(aStep.edge) + "/" + to_string(bStep.edge));
            }
        }
    }

    // Connection splitting is allowed in our generated cfg: multiple PATH records may share the
    // same source/target pair as long as their net_count values sum exactly to the input matrix value.
    for (const Connection &c : d.connections) {
        string key = pairKey(c.from, c.to);
        int seen = routedPairCount[key];
        long long routedSum = routedPairNetSum[key];
        if (seen == 0) {
            vr.routing_errors.push_back("Missing PATH for input connection: " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name);
        } else if (routedSum != c.nets) {
            vr.routing_errors.push_back("PATH net_count sum does not match input matrix for " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name +
                                        ": routed_sum=" + to_string(routedSum) + " expected=" + to_string(c.nets));
        } else if (seen > 1) {
            vr.warnings.push_back("Connection was split into multiple PATH records: " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name +
                                  " pieces=" + to_string(seen));
        }
    }
    set<string> expectedPathKeys;
    for (const Connection &c : d.connections) expectedPathKeys.insert(pairKey(c.from, c.to));
    for (const auto &kv : routedPairNetSum) {
        if (!expectedPathKeys.count(kv.first)) {
            vr.routing_errors.push_back("PATH exists for a pair not present in the input matrix: key=" + kv.first);
        }
    }
    if (vr.total_routed_nets_in_cfg != totalDirectedNets(d)) {
        vr.routing_errors.push_back("Total routed net_count in cfg does not equal total directed nets in input.");
    }

    for (const CfgChannelRecord &c : p.channels) {
        long long load = channelLoads[c.name];
        double capacity = 25.0 * min(c.width, c.height);
        if (load > capacity + EPS) {
            vr.capacity_ok = false;
            vr.overflow_channel_count++;
            vr.capacity_warnings.push_back("Channel capacity warning: " + c.name + " load=" + to_string(load) + " simple_capacity=" + fmt(capacity));
        }
    }
    for (const auto &kv : softFtLoads) {
        if (kv.second > 0) {
            vr.warnings.push_back("Soft feedthrough used: " + kv.first + " ft_nets=" + to_string(kv.second) +
                                  " (debug warning only; final scoring should account for FT area/conversion). ");
        }
    }

    vr.format_ok = vr.format_errors.empty();
    vr.geometry_ok = vr.geometry_errors.empty();
    vr.routing_ok = vr.routing_errors.empty();
    vr.strict_ok = vr.format_ok && vr.geometry_ok && vr.routing_ok;
    vr.overall_pass = vr.strict_ok;
    if (!vr.capacity_ok) {
        vr.warnings.push_back("Capacity overflow exists in the simple self-check; treated as warning for now.");
    }
    return vr;
}


struct PlacementRoutingAttempt {
    string name;
    FullPlacementResult placement;
    ChannelGenerationResult channels;
    RoutingGenerationResult routing;
    double overflow_amount = 0.0;
    int overflow_channels = 0;
    double estimated_cost = 0.0;
    bool valid_for_selection = false;
};

static bool placementBasicOkForSelection(const InputData &d, const FullPlacementResult &pr) {
    if (!pr.errors.empty()) return false;
    for (size_t i = 0; i < d.blocks.size(); ++i) if (!pr.placements[i].placed) return false;
    vector<string> tmp;
    return verifyAllInsideOutline(d, pr, tmp) && verifyNoBlockOverlap(d, pr, tmp);
}

static bool betterPlacementRoutingAttempt(const PlacementRoutingAttempt &a, const PlacementRoutingAttempt &b) {
    if (a.valid_for_selection != b.valid_for_selection) return a.valid_for_selection > b.valid_for_selection;
    if (a.routing.unrouted_count != b.routing.unrouted_count) return a.routing.unrouted_count < b.routing.unrouted_count;
    if (a.routing.unrouted_nets != b.routing.unrouted_nets) return a.routing.unrouted_nets < b.routing.unrouted_nets;
    if (fabs(a.overflow_amount - b.overflow_amount) > 1e-4) return a.overflow_amount < b.overflow_amount;
    if (a.overflow_channels != b.overflow_channels) return a.overflow_channels < b.overflow_channels;
    if (fabs(a.routing.total_estimated_wirelength - b.routing.total_estimated_wirelength) > 1e-4) return a.routing.total_estimated_wirelength < b.routing.total_estimated_wirelength;
    if (fabs(a.estimated_cost - b.estimated_cost) > 1e-4) return a.estimated_cost < b.estimated_cost;
    return a.name < b.name;
}

static vector<PlacementStrategyConfig> demandAwarePlacementStrategies() {
    vector<PlacementStrategyConfig> cfgs;
    PlacementStrategyConfig baseline;
    baseline.name = "BASELINE_PLACEMENT_FIXED_GAP";
    baseline.demand_aware = false;
    baseline.base_gap = 20.0;
    baseline.extra_gap = 0.0;
    baseline.connectivity_weight = 1.0;
    baseline.center_weight = 0.0;
    baseline.compact_weight = 1.0;
    cfgs.push_back(baseline);

    PlacementStrategyConfig compact;
    compact.name = "DEMAND_AWARE_COMPACT";
    compact.demand_aware = true;
    compact.base_gap = 16.0;
    compact.extra_gap = 45.0;
    compact.connectivity_weight = 1.0;
    compact.center_weight = 0.15;
    compact.compact_weight = 1.0;
    cfgs.push_back(compact);

    PlacementStrategyConfig balanced;
    balanced.name = "DEMAND_AWARE_BALANCED";
    balanced.demand_aware = true;
    balanced.base_gap = 24.0;
    balanced.extra_gap = 90.0;
    balanced.connectivity_weight = 0.85;
    balanced.center_weight = 0.35;
    balanced.compact_weight = 0.8;
    cfgs.push_back(balanced);

    PlacementStrategyConfig spacious;
    spacious.name = "DEMAND_AWARE_SPACIOUS";
    spacious.demand_aware = true;
    spacious.base_gap = 32.0;
    spacious.extra_gap = 140.0;
    spacious.connectivity_weight = 0.75;
    spacious.center_weight = 0.45;
    spacious.compact_weight = 0.65;
    cfgs.push_back(spacious);
    return cfgs;
}

static PlacementRoutingAttempt makePlacementRoutingAttempt(const InputData &d,
                                                           const ShapeResult &shapes,
                                                           const EdgePlacementResult &edgePlacement,
                                                           const PlacementStrategyConfig &cfg) {
    PlacementRoutingAttempt a;
    a.name = cfg.name;
    a.placement = placeAllBlocksAfterEdgesWithStrategy(d, shapes, edgePlacement, cfg);
    if (placementBasicOkForSelection(d, a.placement)) {
        a.channels = generateVerticalSliceChannels(d, a.placement);
        if (a.channels.errors.empty()) {
            a.routing = routeAllConnectionsHybridLoadAwareChannelOnly(d, a.placement, a.channels);
            vector<ChannelOverflowDetail> report = buildDetailedChannelOverflowReport(d, a.channels, a.routing);
            a.overflow_amount = totalOverflowAmount(report);
            a.overflow_channels = countOverflowChannels(report);
            a.estimated_cost = d.outline_width * d.outline_height + d.alpha * a.routing.total_estimated_wirelength;
            a.valid_for_selection = a.routing.errors.empty();
        }
    }
    return a;
}

static PlacementRoutingAttempt chooseDemandAwarePlacementAndRouting(const InputData &d,
                                                                    const ShapeResult &shapes,
                                                                    const EdgePlacementResult &edgePlacement) {
    vector<PlacementRoutingAttempt> attempts;
    for (const PlacementStrategyConfig &cfg : demandAwarePlacementStrategies()) {
        attempts.push_back(makePlacementRoutingAttempt(d, shapes, edgePlacement, cfg));
    }
    int best = 0;
    for (int i = 1; i < static_cast<int>(attempts.size()); ++i) {
        if (betterPlacementRoutingAttempt(attempts[i], attempts[best])) best = i;
    }
    PlacementRoutingAttempt chosen = attempts[best];

    stringstream ss;
    ss << "PLACEMENT_ROUTING_SELECTED: " << chosen.name
       << " overflow=" << fmt(chosen.overflow_amount)
       << " overflow_channels=" << chosen.overflow_channels
       << " unrouted=" << chosen.routing.unrouted_count
       << " wirelength=" << fmt(chosen.routing.total_estimated_wirelength);
    chosen.placement.warnings.push_back(ss.str());
    chosen.placement.warnings.push_back("PLACEMENT_ROUTING_COMPARISON: name,valid,unrouted,total_overflow,overflow_channels,wirelength,channel_count");
    for (const PlacementRoutingAttempt &a : attempts) {
        stringstream row;
        row << "  " << a.name << "," << (a.valid_for_selection ? "YES" : "NO")
            << "," << a.routing.unrouted_count
            << "," << fmt(a.overflow_amount)
            << "," << a.overflow_channels
            << "," << fmt(a.routing.total_estimated_wirelength)
            << "," << a.channels.channels.size();
        chosen.placement.warnings.push_back(row.str());
    }
    return chosen;
}


struct FtResizeEntry {
    string block_name;
    long long pass1_ft_nets = 0;
    long long budget_ft_nets = 0;
    long long final_ft_nets = 0;
    double conversion_rate = 0.0;
    double grow_per_side = 0.0;
    double old_width = 0.0;
    double old_height = 0.0;
    double new_width = 0.0;
    double new_height = 0.0;
    double old_area = 0.0;
    double new_area = 0.0;
    double area_delta = 0.0;
    bool aspect_ok = true;
    bool outline_fit_ok = true;
    bool final_ft_covered_by_budget = true;
};

struct FtResizeResult {
    ShapeResult resized_shapes;
    vector<FtResizeEntry> entries;
    long long pass1_total_ft_nets = 0;
    long long budget_total_ft_nets = 0;
    long long final_total_ft_nets = 0;
    double total_area_delta = 0.0;
    vector<string> warnings;
    vector<string> errors;
};

struct TwoPassFtResult {
    ShapeResult resized_shapes;
    PlacementRoutingAttempt pass1;
    PlacementRoutingAttempt pass2;
    FtResizeResult resize;
};

static int ftConversionBucketIndex(long long nets) {
    if (nets <= 0) return -1;
    if (nets <= 3000) return 0;
    if (nets <= 6000) return 1;
    if (nets <= 9000) return 2;
    return 3;
}

static long long ftBudgetNetsFromPass1(long long pass1Nets) {
    // The PDF example applies the conversion rate selected by the net-count bucket,
    // but multiplies by the actual feedthrough net count, not by the bucket upper bound.
    return max(0LL, pass1Nets);
}

static double ftConversionRateForBudget(const Block &b, long long budgetNets) {
    int bi = ftConversionBucketIndex(budgetNets);
    if (bi < 0) return 0.0;
    if (bi >= 0 && bi < static_cast<int>(b.ft_conversion.size())) return b.ft_conversion[bi];
    return 1.0;
}

static FtResizeResult applyTwoPassFtResizeFromPass1Loads(const InputData &d,
                                                         const ShapeResult &baseShapes,
                                                         const vector<long long> &pass1FtLoads) {
    FtResizeResult fr;
    fr.resized_shapes = baseShapes;
    fr.entries.clear();

    for (size_t i = 0; i < d.blocks.size(); ++i) {
        const Block &b = d.blocks[i];
        if (b.type != "SOFT") continue;
        long long pass1Nets = (i < pass1FtLoads.size()) ? pass1FtLoads[i] : 0;
        long long budgetNets = ftBudgetNetsFromPass1(pass1Nets);
        double rate = ftConversionRateForBudget(b, budgetNets);

        FtResizeEntry e;
        e.block_name = b.name;
        e.pass1_ft_nets = pass1Nets;
        e.budget_ft_nets = budgetNets;
        e.conversion_rate = rate;
        e.old_width = baseShapes.shapes[i].width;
        e.old_height = baseShapes.shapes[i].height;
        e.old_area = e.old_width * e.old_height;
        e.new_width = e.old_width;
        e.new_height = e.old_height;
        e.new_area = e.old_area;

        if (budgetNets > 0) {
            double extraTotal = (static_cast<double>(budgetNets) / 25.0) * rate;
            e.grow_per_side = extraTotal / 2.0;
            e.new_width = e.old_width + e.grow_per_side;
            e.new_height = e.old_height + e.grow_per_side;
            e.new_area = e.new_width * e.new_height;
            e.area_delta = e.new_area - e.old_area;

            BlockShape ns = fr.resized_shapes.shapes[i];
            ns.width = e.new_width;
            ns.height = e.new_height;
            ns.area = e.new_area;
            ns.aspect = (ns.height > EPS ? ns.width / ns.height : 0.0);
            ns.fixed_dim_ok = true;
            ns.area_ok = ns.area + max(1.0, b.area) * 1e-4 >= b.area;
            ns.aspect_ok = b.has_ar && ns.aspect + EPS >= b.ar_min && ns.aspect <= b.ar_max + EPS;
            ns.single_outline_fit_ok = (ns.width <= d.outline_width + EPS && ns.height <= d.outline_height + EPS);
            fr.resized_shapes.shapes[i] = ns;
            e.aspect_ok = ns.aspect_ok;
            e.outline_fit_ok = ns.single_outline_fit_ok;
            fr.total_area_delta += e.area_delta;
        }

        fr.pass1_total_ft_nets += pass1Nets;
        fr.budget_total_ft_nets += budgetNets;
        if (!e.aspect_ok) fr.errors.push_back(b.name + " FT-resized SOFT aspect ratio is illegal: aspect=" + fmt(fr.resized_shapes.shapes[i].aspect));
        if (!e.outline_fit_ok) fr.errors.push_back(b.name + " FT-resized SOFT shape cannot fit inside outline individually: " + fmt(e.new_width) + "x" + fmt(e.new_height));
        if (budgetNets > 0 || pass1Nets > 0) fr.entries.push_back(e);
    }

    fr.warnings.push_back("FT_RESIZE_METHOD: two-pass estimate from Pass-1 SOFT feedthrough loads; conversion rate comes from the FT bucket, size increase uses actual Pass-1 FT net count.");
    return fr;
}

static void updateFtResizeResultWithFinalLoads(const InputData &d,
                                               const vector<long long> &finalFtLoads,
                                               FtResizeResult &fr) {
    map<string, size_t> entryIndex;
    for (size_t ei = 0; ei < fr.entries.size(); ++ei) entryIndex[fr.entries[ei].block_name] = ei;
    fr.final_total_ft_nets = 0;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (d.blocks[i].type != "SOFT") continue;
        long long finalNets = (i < finalFtLoads.size()) ? finalFtLoads[i] : 0;
        fr.final_total_ft_nets += finalNets;
        auto it = entryIndex.find(d.blocks[i].name);
        if (it == entryIndex.end()) {
            if (finalNets > 0) {
                FtResizeEntry e;
                e.block_name = d.blocks[i].name;
                e.pass1_ft_nets = 0;
                e.budget_ft_nets = 0;
                e.final_ft_nets = finalNets;
                e.old_width = fr.resized_shapes.shapes[i].width;
                e.old_height = fr.resized_shapes.shapes[i].height;
                e.new_width = e.old_width;
                e.new_height = e.old_height;
                e.old_area = e.old_width * e.old_height;
                e.new_area = e.old_area;
                e.final_ft_covered_by_budget = false;
                fr.entries.push_back(e);
                fr.warnings.push_back(d.blocks[i].name + " has final-pass FT nets but had zero Pass-1 FT budget; a third pass may improve consistency.");
            }
        } else {
            FtResizeEntry &e = fr.entries[it->second];
            e.final_ft_nets = finalNets;
            e.final_ft_covered_by_budget = (finalNets <= e.budget_ft_nets);
            if (!e.final_ft_covered_by_budget) {
                fr.warnings.push_back(e.block_name + " final FT nets=" + to_string(finalNets) + " exceed Pass-1 budget=" + to_string(e.budget_ft_nets) + "; a third pass would resize it further.");
            }
        }
    }
}

static TwoPassFtResult chooseTwoPassFtResizedPlacementAndRouting(const InputData &d,
                                                                 const ShapeResult &baseShapes,
                                                                 const EdgePlacementResult &edgePlacement) {
    TwoPassFtResult t;
    t.pass1 = chooseDemandAwarePlacementAndRouting(d, baseShapes, edgePlacement);
    t.resize = applyTwoPassFtResizeFromPass1Loads(d, baseShapes, t.pass1.routing.soft_ft_loads);
    t.resized_shapes = t.resize.resized_shapes;
    t.pass2 = chooseDemandAwarePlacementAndRouting(d, t.resized_shapes, edgePlacement);
    updateFtResizeResultWithFinalLoads(d, t.pass2.routing.soft_ft_loads, t.resize);

    t.pass2.placement.warnings.push_back("TWO_PASS_FT_RESIZE_PASS1_SELECTED: " + t.pass1.name +
        " ft_nets=" + to_string(t.resize.pass1_total_ft_nets) +
        " overflow=" + fmt(t.pass1.overflow_amount) +
        " wl=" + fmt(t.pass1.routing.total_estimated_wirelength));
    t.pass2.placement.warnings.push_back("TWO_PASS_FT_RESIZE_PASS2_SELECTED: " + t.pass2.name +
        " final_ft_nets=" + to_string(t.resize.final_total_ft_nets) +
        " budget_ft_nets=" + to_string(t.resize.budget_total_ft_nets) +
        " added_soft_area=" + fmt(t.resize.total_area_delta) +
        " overflow=" + fmt(t.pass2.overflow_amount) +
        " wl=" + fmt(t.pass2.routing.total_estimated_wirelength));
    for (const string &w : t.resize.warnings) t.pass2.placement.warnings.push_back(w);
    for (const string &e : t.resize.errors) t.pass2.placement.errors.push_back(e);
    return t;
}

static void printStep16CfgVerification(const string &filename,
                                      const InputData &d,
                                      const ShapeResult &sr,
                                      const EdgePlacementResult &er,
                                      const FullPlacementResult &pr,
                                      const ChannelGenerationResult &cr,
                                      const RoutingGenerationResult &rr,
                                      const CfgWriteResult &cfg,
                                      const CfgValidationResult &cv,
                                      const FtResizeResult &fr) {
    vector<string> verifyErrors;

    bool parserOk = d.errors.empty();
    bool shapeOk = sr.errors.empty();

    bool allPlaced = true;
    for (size_t i = 0; i < d.blocks.size(); ++i) {
        if (!pr.placements[i].placed) allPlaced = false;
    }

    bool insideOk = verifyAllInsideOutline(d, pr, verifyErrors);
    bool edgeConstraintOk = verifyEdgeConstraintsInFullPlacement(d, pr, er, verifyErrors);
    bool noOverlapOk = verifyNoBlockOverlap(d, pr, verifyErrors);
    bool channelPositiveOk = verifyChannelsPositiveSize(cr, verifyErrors);
    bool channelInsideOk = verifyChannelsInsideOutline(d, cr, verifyErrors);
    bool channelBlockOverlapOk = verifyChannelsDoNotOverlapBlocks(d, pr, cr, verifyErrors);
    bool channelOverlapOk = verifyChannelsDoNotOverlapEachOther(cr, verifyErrors);
    bool channelAreaOk = verifyChannelAreaCoverage(d, pr, cr, verifyErrors);
    bool channelOk = cr.errors.empty() && channelPositiveOk && channelInsideOk && channelBlockOverlapOk && channelOverlapOk && channelAreaOk;
    bool placementOk = er.errors.empty() && pr.errors.empty() && allPlaced && insideOk && edgeConstraintOk && noOverlapOk;

    bool routedAllOk = verifyAllConnectionsRouted(d, rr, verifyErrors);
    bool channelOrSoftFtOk = verifyRouteIntermediateChannelOrSoftFeedthrough(d, rr, verifyErrors);
    bool pathFormatOk = verifyPathStepFormat(d, cr, rr, verifyErrors);
    bool netCountOk = verifyRoutedNetCount(d, rr, verifyErrors);
    bool routingOk = rr.errors.empty() && routedAllOk && channelOrSoftFtOk && pathFormatOk && netCountOk;

    double outlineArea = d.outline_width * d.outline_height;
    double estimatedCost = outlineArea + d.alpha * rr.total_estimated_wirelength;

    cout << "==============================\n";
    cout << "PROGRAM_VERSION: STEP_16_TWO_PASS_FT_RESIZE\n";
    cout << "INPUT_FILE: " << filename << "\n";
    cout << "CFG_OUTPUT_FILE: " << cfg.output_path << "\n";
    cout << "CFG_WRITTEN: " << (cfg.written ? "YES" : "NO") << "\n";
    cout << "VERIFY_STEP: 16 - two-pass SOFT feedthrough size recalculation, then re-placement/re-routing\n";
    cout << "BLOCK_COUNT: " << d.blocks.size()
         << "  SOFT: " << countType(d, "SOFT")
         << "  MACRO: " << countType(d, "MACRO")
         << "  EDGE: " << countType(d, "EDGE") << "\n";
    cout << "OUTLINE_MAX: " << fmt(d.outline_width) << " x " << fmt(d.outline_height) << "\n";
    cout << "OUTLINE_AREA: " << fmt(outlineArea) << "\n";
    cout << "TOTAL_PLACED_BLOCK_AREA: " << fmt(totalPlacedArea(d, pr)) << "\n";
    cout << "PLACEMENT_STRATEGY: " << (pr.strategy_name.empty() ? string("UNKNOWN") : pr.strategy_name) << "\n";
    cout << "CHANNEL_COUNT: " << cr.channels.size() << "\n";
    cout << "TOTAL_CHANNEL_AREA: " << fmt(cr.channel_area) << "\n";
    cout << "ALPHA: " << fmt(d.alpha) << "\n";
    cout << "NONZERO_CONNECTIONS: " << d.connections.size() << "\n";
    cout << "TOTAL_DIRECTED_NETS: " << totalDirectedNets(d) << "\n";
    cout << "ROUTED_CONNECTIONS: " << rr.routed_count << "\n";
    cout << "UNROUTED_CONNECTIONS: " << rr.unrouted_count << "\n";
    cout << "ROUTED_NETS: " << rr.routed_nets << "\n";
    cout << "UNROUTED_NETS: " << rr.unrouted_nets << "\n";
    cout << "PORT_RELAXED_ROUTES: " << rr.port_relaxed_count << "\n";
    cout << "EST_TOTAL_WIRELENGTH_BY_GUIDING_POINTS: " << fmt(rr.total_estimated_wirelength) << "\n";
    cout << "EST_COST: " << fmt(estimatedCost) << "\n";

    cout << "\nSTEP_STATUS:\n";
    cout << "  PARSER_CHECK: " << (parserOk ? "PASS" : "FAIL") << "\n";
    cout << "  SHAPE_GENERATION_CHECK: " << (shapeOk ? "PASS" : "FAIL") << "\n";
    cout << "  FULL_PLACEMENT_CHECK: " << (placementOk ? "PASS" : "FAIL") << "\n";
    cout << "  CHANNEL_GENERATION_CHECK: " << (channelOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_GENERATION_CHECK: " << (rr.errors.empty() ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_ALL_CONNECTIONS_ROUTED_CHECK: " << (routedAllOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_INTERMEDIATE_CHANNEL_OR_SOFT_FT_CHECK: " << (channelOrSoftFtOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_PATH_EDGE_FORMAT_CHECK: " << (pathFormatOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_NET_COUNT_CHECK: " << (netCountOk ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_GENERATION_CHECK: " << ((cfg.written && cfg.errors.empty()) ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_FORMAT_SELF_CHECK: " << (cv.format_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_GEOMETRY_SELF_CHECK: " << (cv.geometry_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_ROUTING_SELF_CHECK: " << (cv.routing_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_CAPACITY_SELF_CHECK: " << (cv.capacity_ok ? "PASS" : "WARN") << "\n";
    cout << "  CFG_STRICT_VALIDITY_SELF_CHECK: " << (cv.strict_ok ? "PASS" : "FAIL") << "\n";

    cout << "\nCFG_OUTPUT_SUMMARY:\n";
    cout << "  output_file: " << cfg.output_path << "\n";
    cout << "  block_records: " << cfg.block_count << "\n";
    cout << "  channel_records: " << cfg.channel_count << "\n";
    cout << "  path_records: " << cfg.path_count << "\n";

    cout << "\nCFG_SELF_CHECK_SUMMARY:\n";
    cout << "  format_check: " << (cv.format_ok ? "PASS" : "FAIL") << "\n";
    cout << "  geometry_check: " << (cv.geometry_ok ? "PASS" : "FAIL") << "\n";
    cout << "  routing_check: " << (cv.routing_ok ? "PASS" : "FAIL") << "\n";
    cout << "  capacity_check: " << (cv.capacity_ok ? "PASS" : "WARN") << "\n";
    cout << "  strict_validity_check: " << (cv.strict_ok ? "PASS" : "FAIL") << "\n";
    cout << "  cfg_total_routed_nets: " << cv.total_routed_nets_in_cfg << "\n";
    cout << "  overflow_channel_count_simple: " << cv.overflow_channel_count << "\n";

    cout << "\nROUTING_PATTERNS_READY_FOR_CFG:\n";
    cout << "  # Same PATH syntax that will be used later in the .cfg PATH section.\n";
    for (const RoutePattern &rp : rr.routes) {
        if (rp.routed) {
            cout << "  " << routePathAsText(rp);
            if (rp.used_port_relaxation) cout << "  # PORT_RELAXED";
            cout << "\n";
        } else {
            cout << "  # UNROUTED " << d.blocks[rp.from].name << " -> " << d.blocks[rp.to].name
                 << " nets=" << rp.nets << " reason=" << rp.fail_reason << "\n";
        }
    }

    cout << "\nROUTE_NODE_SUMMARY:\n";
    cout << "  from,to,nets,node_count,channel_count/softft_count,estimated_wirelength,status\n";
    for (const RoutePattern &rp : rr.routes) {
        int chCount = 0;
        int softFtCount = 0;
        int nBlocksLocal = static_cast<int>(d.blocks.size());
        for (size_t ni = 0; ni < rp.nodes.size(); ++ni) {
            int node = rp.nodes[ni];
            if (isChannelNode(node, nBlocksLocal)) chCount++;
            else if (ni > 0 && ni + 1 < rp.nodes.size() && node >= 0 && node < nBlocksLocal && d.blocks[node].type == "SOFT") softFtCount++;
        }
        cout << "  " << d.blocks[rp.from].name << "," << d.blocks[rp.to].name << "," << rp.nets << ","
             << rp.nodes.size() << "," << chCount << "/softft=" << softFtCount << "," << fmt(rp.estimated_wirelength) << ","
             << (rp.routed ? (rp.used_port_relaxation ? "ROUTED_PORT_RELAXED" : "ROUTED") : "UNROUTED") << "\n";
    }

    vector<ChannelOverflowDetail> overflowReport = buildDetailedChannelOverflowReport(d, cr, rr);
    int overflowCount = countOverflowChannels(overflowReport);
    double totalOverflow = totalOverflowAmount(overflowReport);

    cout << "\nSOFT_FEEDTHROUGH_SUMMARY:\n";
    long long totalSoftFt = 0;
    int softFtBlockCount = 0;
    if (!rr.soft_ft_loads.empty()) {
        for (size_t bi = 0; bi < d.blocks.size() && bi < rr.soft_ft_loads.size(); ++bi) {
            if (d.blocks[bi].type == "SOFT" && rr.soft_ft_loads[bi] > 0) {
                totalSoftFt += rr.soft_ft_loads[bi];
                softFtBlockCount++;
            }
        }
    }
    cout << "  soft_blocks_used=" << softFtBlockCount << " total_soft_ft_nets=" << totalSoftFt << "\n";
    if (!rr.soft_ft_loads.empty()) {
        for (size_t bi = 0; bi < d.blocks.size() && bi < rr.soft_ft_loads.size(); ++bi) {
            if (d.blocks[bi].type == "SOFT" && rr.soft_ft_loads[bi] > 0) {
                cout << "  " << d.blocks[bi].name << " ft_nets=" << rr.soft_ft_loads[bi]
                     << " capacity_estimate=" << fmt(softFeedthroughCapacityEstimate(d, pr, static_cast<int>(bi))) << "\n";
            }
        }
    }


    cout << "\nFT_SIZE_RECALCULATION_REPORT:\n";
    cout << "  method: two-pass Pass-1 FT load -> resize SOFT blocks -> Pass-2 place/route/output\n";
    cout << "  pass1_total_ft_nets=" << fr.pass1_total_ft_nets
         << " budget_total_ft_nets=" << fr.budget_total_ft_nets
         << " final_total_ft_nets=" << fr.final_total_ft_nets
         << " total_soft_area_delta=" << fmt(fr.total_area_delta) << "\n";
    cout << "  block,pass1_ft,budget_ft,final_ft,rate,grow_per_side,old_w,old_h,new_w,new_h,area_delta,aspect_ok,outline_fit,final_covered\n";
    if (fr.entries.empty()) {
        cout << "  NONE\n";
    } else {
        for (const FtResizeEntry &e : fr.entries) {
            cout << "  " << e.block_name << "," << e.pass1_ft_nets << "," << e.budget_ft_nets << "," << e.final_ft_nets
                 << "," << fmt(e.conversion_rate) << "," << fmt(e.grow_per_side)
                 << "," << fmt(e.old_width) << "," << fmt(e.old_height)
                 << "," << fmt(e.new_width) << "," << fmt(e.new_height)
                 << "," << fmt(e.area_delta)
                 << "," << (e.aspect_ok ? "YES" : "NO")
                 << "," << (e.outline_fit_ok ? "YES" : "NO")
                 << "," << (e.final_ft_covered_by_budget ? "YES" : "NO") << "\n";
        }
    }

    cout << "\nCHANNEL_CAPACITY_SUMMARY_DETAILED:\n";
    cout << "  # Capacity model for this report: 25 * min(channel_width, channel_height).\n";
    cout << "  # This is a conservative debug report to locate bottleneck channels; capacity is still WARN, not FAIL.\n";
    cout << "  overloaded_channels_simple: " << overflowCount << " / " << overflowReport.size() << "\n";
    cout << "  total_overflow_nets_simple: " << fmt(totalOverflow) << "\n";
    if (!overflowReport.empty()) {
        const ChannelOverflowDetail &worstOverflow = overflowReport.front();
        auto worstUtilIt = max_element(overflowReport.begin(), overflowReport.end(), [](const ChannelOverflowDetail &a, const ChannelOverflowDetail &b) {
            return a.utilization < b.utilization;
        });
        cout << "  worst_by_overflow: " << worstOverflow.name
             << " load=" << worstOverflow.load
             << " cap=" << fmt(worstOverflow.capacity)
             << " overflow=" << fmt(worstOverflow.overflow)
             << " util=" << fmt(worstOverflow.utilization) << "\n";
        if (worstUtilIt != overflowReport.end()) {
            cout << "  worst_by_utilization: " << worstUtilIt->name
                 << " load=" << worstUtilIt->load
                 << " cap=" << fmt(worstUtilIt->capacity)
                 << " overflow=" << fmt(worstUtilIt->overflow)
                 << " util=" << fmt(worstUtilIt->utilization) << "\n";
        }
    }

    cout << "\nCHANNEL_OVERFLOW_REPORT_DETAILED:\n";
    cout << "  # Sorted by overflow first, then utilization. top_contributors shows the largest net-count routes using that channel.\n";
    cout << "  channel,lx,ly,width,height,load,capacity,overflow,utilization,path_count,top_contributors\n";
    bool printedOverflow = false;
    for (const ChannelOverflowDetail &r : overflowReport) {
        if (r.overflow <= EPS) continue;
        printedOverflow = true;
        cout << "  " << r.name << "," << fmt(r.lx) << "," << fmt(r.ly) << ","
             << fmt(r.width) << "," << fmt(r.height) << "," << r.load << ","
             << fmt(r.capacity) << "," << fmt(r.overflow) << "," << fmt(r.utilization) << ","
             << r.path_count << "," << contributorSummary(r.contributors, 6) << "\n";
    }
    if (!printedOverflow) cout << "  NONE\n";

    cout << "\nCHANNEL_LOAD_REPORT_SORTED_ALL:\n";
    cout << "  # Includes non-overflow channels too, sorted by overflow/utilization.\n";
    cout << "  channel,lx,ly,width,height,load,capacity,overflow,utilization,path_count\n";
    for (const ChannelOverflowDetail &r : overflowReport) {
        cout << "  " << r.name << "," << fmt(r.lx) << "," << fmt(r.ly) << ","
             << fmt(r.width) << "," << fmt(r.height) << "," << r.load << ","
             << fmt(r.capacity) << "," << fmt(r.overflow) << "," << fmt(r.utilization) << ","
             << r.path_count << "\n";
    }

    if (!d.warnings.empty() || !sr.warnings.empty() || !er.warnings.empty() || !pr.warnings.empty() || !cr.warnings.empty() || !rr.warnings.empty() || !cfg.warnings.empty() || !cv.warnings.empty() || !cv.capacity_warnings.empty() || !fr.warnings.empty()) {
        cout << "\nWARNINGS:\n";
        for (const string &w : d.warnings) cout << "  - " << w << "\n";
        for (const string &w : sr.warnings) cout << "  - " << w << "\n";
        for (const string &w : er.warnings) cout << "  - " << w << "\n";
        for (const string &w : pr.warnings) cout << "  - " << w << "\n";
        for (const string &w : cr.warnings) cout << "  - " << w << "\n";
        for (const string &w : rr.warnings) cout << "  - " << w << "\n";
        for (const string &w : cfg.warnings) cout << "  - " << w << "\n";
        for (const string &w : cv.warnings) cout << "  - " << w << "\n";
        for (const string &w : fr.warnings) cout << "  - " << w << "\n";
        size_t capWarnLimit = min<size_t>(20, cv.capacity_warnings.size());
        for (size_t wi = 0; wi < capWarnLimit; ++wi) cout << "  - " << cv.capacity_warnings[wi] << "\n";
        if (cv.capacity_warnings.size() > capWarnLimit) cout << "  - ... " << (cv.capacity_warnings.size() - capWarnLimit) << " more capacity warnings omitted from console output.\n";
    }

    if (!d.errors.empty() || !sr.errors.empty() || !er.errors.empty() || !pr.errors.empty() || !cr.errors.empty() || !rr.errors.empty() || !cfg.errors.empty() || !verifyErrors.empty() || !cv.format_errors.empty() || !cv.geometry_errors.empty() || !cv.routing_errors.empty() || !fr.errors.empty()) {
        cout << "\nERRORS:\n";
        for (const string &e : d.errors) cout << "  - " << e << "\n";
        for (const string &e : sr.errors) cout << "  - " << e << "\n";
        for (const string &e : er.errors) cout << "  - " << e << "\n";
        for (const string &e : pr.errors) cout << "  - " << e << "\n";
        for (const string &e : cr.errors) cout << "  - " << e << "\n";
        for (const string &e : rr.errors) cout << "  - " << e << "\n";
        for (const string &e : cfg.errors) cout << "  - " << e << "\n";
        for (const string &e : verifyErrors) cout << "  - " << e << "\n";
        for (const string &e : cv.format_errors) cout << "  - CFG_FORMAT: " << e << "\n";
        for (const string &e : cv.geometry_errors) cout << "  - CFG_GEOMETRY: " << e << "\n";
        for (const string &e : cv.routing_errors) cout << "  - CFG_ROUTING: " << e << "\n";
        for (const string &e : fr.errors) cout << "  - FT_RESIZE: " << e << "\n";
    }

    cout << "\nRESULT: " << (parserOk && shapeOk && placementOk && channelOk && routingOk && cfg.written && cfg.errors.empty() && fr.errors.empty() && cv.overall_pass ? (cv.capacity_ok ? "PASS" : "PASS_WITH_CAPACITY_WARNINGS") : "FAIL") << "\n";
}

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <input_case.csv> [more_case.csv ...]\n";
        cerr << "This Step-16 version does two-pass SOFT feedthrough size recalculation, writes a .cfg file, then validates the generated .cfg format/geometry/routing.\n";
        return 1;
    }

    bool anyFail = false;
    for (int i = 1; i < argc; ++i) {
        string filename = argv[i];
        InputData data = parseInputCsv(filename);
        ShapeResult baseShapes = generateLegalBlockShapes(data);
        EdgePlacementResult edgePlacement = placeEdgeBlocksFirst(data, baseShapes);
        TwoPassFtResult twoPass = chooseTwoPassFtResizedPlacementAndRouting(data, baseShapes, edgePlacement);
        ShapeResult finalShapes = twoPass.resized_shapes;
        FullPlacementResult fullPlacement = twoPass.pass2.placement;
        ChannelGenerationResult channels = twoPass.pass2.channels;
        RoutingGenerationResult routing = twoPass.pass2.routing;
        CfgWriteResult cfg = writeCfgFile(filename, data, fullPlacement, channels, routing);
        CfgValidationResult cfgValidation = validateGeneratedCfgFile(cfg.output_path, data);
        printStep16CfgVerification(filename, data, finalShapes, edgePlacement, fullPlacement, channels, routing, cfg, cfgValidation, twoPass.resize);
        if (!data.errors.empty() || !finalShapes.errors.empty() || !edgePlacement.errors.empty() || !fullPlacement.errors.empty() || !channels.errors.empty() || !routing.errors.empty() || !cfg.errors.empty() || !cfg.written || !twoPass.resize.errors.empty() || !cfgValidation.overall_pass) anyFail = true;
    }
    return anyFail ? 2 : 0;
}
