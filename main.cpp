#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <queue>
#include <numeric>
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
                                                         const vector<Rect> &placed) {
    vector<Rect> candidates;
    if (w <= 0.0 || h <= 0.0 || w > outlineW + EPS || h > outlineH + EPS) return candidates;

    // Keep a small whitespace moat around every non-edge block placement.
    // This makes the generated channel graph much less likely to contain isolated pockets.
    // The value is intentionally small compared with testcase dimensions, but positive enough
    // to create legal channel rectangles and block-channel adjacency.
    const double routingGap = 20.0;
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

static FullPlacementResult placeAllBlocksAfterEdges(const InputData &d, const ShapeResult &sr, const EdgePlacementResult &er) {
    FullPlacementResult pr;
    pr.placements.assign(d.blocks.size(), FullBlockPlacement{});

    vector<Rect> placed;
    vector<Rect> placedByIndex(d.blocks.size());

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

    // Place larger and more constrained blocks first. MACRO keeps fixed shape, so it has priority.
    sort(innerIdx.begin(), innerIdx.end(), [&](int a, int b) {
        bool am = d.blocks[a].type == "MACRO";
        bool bm = d.blocks[b].type == "MACRO";
        if (am != bm) return am > bm;
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
        vector<Rect> candidates = generateInteriorPlacementCandidates(b.name, s.width, s.height, d.outline_width, d.outline_height, placed);

        if (candidates.empty()) {
            pr.errors.push_back(b.name + " cannot be placed inside outline without overlap. shape=" + fmt(s.width) + "x" + fmt(s.height));
            continue;
        }

        // Default: compact bottom-left packing. If this block already connects to placed blocks,
        // use connectivity as the first objective to get a slightly better baseline layout.
        double connWeight = connectivityWeightToPlaced(d, idx, placedByIndex);
        sort(candidates.begin(), candidates.end(), [&](const Rect &a, const Rect &bRect) {
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
        });

        Rect chosen = candidates.front();
        FullBlockPlacement bp;
        bp.placed = true;
        bp.method = "INTERIOR_PACKED";
        bp.lx = chosen.lx;
        bp.ly = chosen.ly;
        bp.width = chosen.w;
        bp.height = chosen.h;
        pr.placements[idx] = bp;
        placed.push_back(chosen);
        placedByIndex[idx] = chosen;
        pr.placement_order.push_back(idx);
    }

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

static RoutingGenerationResult routeAllConnectionsThroughChannelsOnly(const InputData &d,
                                                                       const FullPlacementResult &pr,
                                                                       const ChannelGenerationResult &cr) {
    RoutingGenerationResult rr;
    rr.channel_loads.assign(cr.channels.size(), 0);
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
            // For this baseline step, still try to close every route through channels.
            // We record this as a warning because final submission may need stricter port handling.
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
        }
        rr.routes.push_back(rp);
    }
    return rr;
}

static bool verifyAllConnectionsRouted(const InputData &d, const RoutingGenerationResult &rr, vector<string> &errors) {
    bool ok = true;
    if (rr.routes.size() != d.connections.size()) {
        errors.push_back("Route pattern count does not match nonzero connection count.");
        ok = false;
    }
    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) {
            errors.push_back("Missing route for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
            ok = false;
        }
    }
    return ok;
}

static bool verifyRouteIntermediateChannelOnly(const InputData &d, const RoutingGenerationResult &rr, vector<string> &errors) {
    int nBlocks = static_cast<int>(d.blocks.size());
    bool ok = true;
    for (const RoutePattern &rp : rr.routes) {
        if (!rp.routed) continue;
        if (rp.nodes.size() < 3) {
            errors.push_back("Route " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name + " does not pass through any channel.");
            ok = false;
            continue;
        }
        if (rp.nodes.front() != rp.from || rp.nodes.back() != rp.to) {
            errors.push_back("Route endpoint mismatch for " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name);
            ok = false;
        }
        for (size_t i = 1; i + 1 < rp.nodes.size(); ++i) {
            if (!isChannelNode(rp.nodes[i], nBlocks)) {
                errors.push_back("Route " + d.blocks[rp.from].name + " -> " + d.blocks[rp.to].name + " has non-channel intermediate node.");
                ok = false;
            }
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
            } else if (key == "BLOCK") {
                if (parsed.saw_block_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated BLOCK section.");
                parsed.saw_block_section = true;
                section = Section::BLOCK;
            } else if (key == "CHANNEL") {
                if (parsed.saw_channel_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated CHANNEL section.");
                parsed.saw_channel_section = true;
                section = Section::CHANNEL;
            } else if (key == "PATH") {
                if (parsed.saw_path_section) formatErrors.push_back("Line " + to_string(lineNo) + ": duplicated PATH section.");
                parsed.saw_path_section = true;
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

    if (parsed.declared_blocks < 0) formatErrors.push_back("Missing NumBlocks line.");
    else if (parsed.declared_blocks != static_cast<int>(parsed.blocks.size())) formatErrors.push_back("NumBlocks does not match actual BLOCK records.");
    if (parsed.declared_channels < 0) formatErrors.push_back("Missing NumChannels line.");
    else if (parsed.declared_channels != static_cast<int>(parsed.channels.size())) formatErrors.push_back("NumChannels does not match actual CHANNEL records.");
    if (parsed.declared_paths < 0) formatErrors.push_back("Missing NumRoutingPattern line.");
    else if (parsed.declared_paths != static_cast<int>(parsed.paths.size())) formatErrors.push_back("NumRoutingPattern does not match actual PATH records.");

    return parsed;
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
            if (fabs(area - bi.area) > max(1.0, bi.area) * 1e-4) {
                vr.geometry_errors.push_back(bi.name + " SOFT area mismatch in cfg.");
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
    map<string, long long> channelLoads;

    for (const CfgPathRecord &path : p.paths) {
        vr.total_routed_nets_in_cfg += path.nets;
        if (path.steps.size() < 4 || (path.steps.size() % 2) != 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH must have an even number of rectangle-edge pairs and at least one intermediate rectangle.");
            continue;
        }

        const string &srcName = path.steps.front().rect_name;
        const string &dstName = path.steps.back().rect_name;
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
        if (d.conn[srcIdx][dstIdx] <= 0) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH endpoint pair is not a nonzero input connection: " + srcName + " -> " + dstName);
        } else if (path.nets != d.conn[srcIdx][dstIdx]) {
            vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": PATH net_count does not match input matrix for " + srcName + " -> " + dstName);
        }
        routedPairCount[pairKey(srcIdx, dstIdx)]++;

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
            }
        }

        map<string, bool> touchedChannels;
        for (size_t i = 1; i + 1 < path.steps.size(); ++i) {
            const string &nm = path.steps[i].rect_name;
            if (rectKind.count(nm) && rectKind[nm] != "CHANNEL") {
                vr.routing_errors.push_back("Line " + to_string(path.line_no) + ": intermediate rectangle is not CHANNEL-only: " + nm);
            }
            if (rectKind.count(nm) && rectKind[nm] == "CHANNEL") touchedChannels[nm] = true;
        }
        for (const auto &kv : touchedChannels) channelLoads[kv.first] += path.nets;

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

    if (static_cast<int>(p.paths.size()) != static_cast<int>(d.connections.size())) {
        vr.routing_errors.push_back("PATH record count does not equal nonzero input connection count.");
    }
    for (const Connection &c : d.connections) {
        string key = pairKey(c.from, c.to);
        int seen = routedPairCount[key];
        if (seen == 0) {
            vr.routing_errors.push_back("Missing PATH for input connection: " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name);
        } else if (seen > 1) {
            vr.routing_errors.push_back("Duplicated PATH for input connection: " + d.blocks[c.from].name + " -> " + d.blocks[c.to].name);
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

    vr.format_ok = vr.format_errors.empty();
    vr.geometry_ok = vr.geometry_errors.empty();
    vr.routing_ok = vr.routing_errors.empty();
    vr.overall_pass = vr.format_ok && vr.geometry_ok && vr.routing_ok;
    if (!vr.capacity_ok) {
        vr.warnings.push_back("Capacity overflow exists in the simple self-check; treated as warning for now.");
    }
    return vr;
}

static void printStep7CfgVerification(const string &filename,
                                      const InputData &d,
                                      const ShapeResult &sr,
                                      const EdgePlacementResult &er,
                                      const FullPlacementResult &pr,
                                      const ChannelGenerationResult &cr,
                                      const RoutingGenerationResult &rr,
                                      const CfgWriteResult &cfg,
                                      const CfgValidationResult &cv) {
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
    bool channelOnlyOk = verifyRouteIntermediateChannelOnly(d, rr, verifyErrors);
    bool pathFormatOk = verifyPathStepFormat(d, cr, rr, verifyErrors);
    bool netCountOk = verifyRoutedNetCount(d, rr, verifyErrors);
    bool routingOk = rr.errors.empty() && routedAllOk && channelOnlyOk && pathFormatOk && netCountOk;

    double outlineArea = d.outline_width * d.outline_height;
    double estimatedCost = outlineArea + d.alpha * rr.total_estimated_wirelength;

    cout << "==============================\n";
    cout << "PROGRAM_VERSION: STEP_8_CFG_SELF_CHECKER\n";
    cout << "INPUT_FILE: " << filename << "\n";
    cout << "CFG_OUTPUT_FILE: " << cfg.output_path << "\n";
    cout << "CFG_WRITTEN: " << (cfg.written ? "YES" : "NO") << "\n";
    cout << "VERIFY_STEP: 8 - generate output .cfg file, then run internal validity self-checker\n";
    cout << "BLOCK_COUNT: " << d.blocks.size()
         << "  SOFT: " << countType(d, "SOFT")
         << "  MACRO: " << countType(d, "MACRO")
         << "  EDGE: " << countType(d, "EDGE") << "\n";
    cout << "OUTLINE_MAX: " << fmt(d.outline_width) << " x " << fmt(d.outline_height) << "\n";
    cout << "OUTLINE_AREA: " << fmt(outlineArea) << "\n";
    cout << "TOTAL_PLACED_BLOCK_AREA: " << fmt(totalPlacedArea(d, pr)) << "\n";
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
    cout << "  ROUTING_INTERMEDIATE_CHANNEL_ONLY_CHECK: " << (channelOnlyOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_PATH_EDGE_FORMAT_CHECK: " << (pathFormatOk ? "PASS" : "FAIL") << "\n";
    cout << "  ROUTING_NET_COUNT_CHECK: " << (netCountOk ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_GENERATION_CHECK: " << ((cfg.written && cfg.errors.empty()) ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_FORMAT_SELF_CHECK: " << (cv.format_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_GEOMETRY_SELF_CHECK: " << (cv.geometry_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_ROUTING_SELF_CHECK: " << (cv.routing_ok ? "PASS" : "FAIL") << "\n";
    cout << "  CFG_CAPACITY_SELF_CHECK: " << (cv.capacity_ok ? "PASS" : "WARN") << "\n";

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
    cout << "  from,to,nets,node_count,channel_count,estimated_wirelength,status\n";
    for (const RoutePattern &rp : rr.routes) {
        int chCount = 0;
        for (int node : rp.nodes) if (isChannelNode(node, static_cast<int>(d.blocks.size()))) chCount++;
        cout << "  " << d.blocks[rp.from].name << "," << d.blocks[rp.to].name << "," << rp.nets << ","
             << rp.nodes.size() << "," << chCount << "," << fmt(rp.estimated_wirelength) << ","
             << (rp.routed ? (rp.used_port_relaxation ? "ROUTED_PORT_RELAXED" : "ROUTED") : "UNROUTED") << "\n";
    }

    cout << "\nCHANNEL_LOAD_REPORT_SIMPLE:\n";
    cout << "  # This is only a simple total-net load per channel, not the final official overflow check yet.\n";
    cout << "  channel,lx,ly,width,height,total_passing_nets,pessimistic_capacity_25_per_um_min_dim\n";
    for (size_t i = 0; i < cr.channels.size(); ++i) {
        const ChannelRect &c = cr.channels[i];
        double cap = 25.0 * min(c.width, c.height);
        cout << "  " << c.name << "," << fmt(c.lx) << "," << fmt(c.ly) << ","
             << fmt(c.width) << "," << fmt(c.height) << "," << rr.channel_loads[i] << "," << fmt(cap) << "\n";
    }

    if (!d.warnings.empty() || !sr.warnings.empty() || !er.warnings.empty() || !pr.warnings.empty() || !cr.warnings.empty() || !rr.warnings.empty() || !cfg.warnings.empty() || !cv.warnings.empty() || !cv.capacity_warnings.empty()) {
        cout << "\nWARNINGS:\n";
        for (const string &w : d.warnings) cout << "  - " << w << "\n";
        for (const string &w : sr.warnings) cout << "  - " << w << "\n";
        for (const string &w : er.warnings) cout << "  - " << w << "\n";
        for (const string &w : pr.warnings) cout << "  - " << w << "\n";
        for (const string &w : cr.warnings) cout << "  - " << w << "\n";
        for (const string &w : rr.warnings) cout << "  - " << w << "\n";
        for (const string &w : cfg.warnings) cout << "  - " << w << "\n";
        for (const string &w : cv.warnings) cout << "  - " << w << "\n";
        size_t capWarnLimit = min<size_t>(20, cv.capacity_warnings.size());
        for (size_t wi = 0; wi < capWarnLimit; ++wi) cout << "  - " << cv.capacity_warnings[wi] << "\n";
        if (cv.capacity_warnings.size() > capWarnLimit) cout << "  - ... " << (cv.capacity_warnings.size() - capWarnLimit) << " more capacity warnings omitted from console output.\n";
    }

    if (!d.errors.empty() || !sr.errors.empty() || !er.errors.empty() || !pr.errors.empty() || !cr.errors.empty() || !rr.errors.empty() || !cfg.errors.empty() || !verifyErrors.empty() || !cv.format_errors.empty() || !cv.geometry_errors.empty() || !cv.routing_errors.empty()) {
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
    }

    cout << "\nRESULT: " << (parserOk && shapeOk && placementOk && channelOk && routingOk && cfg.written && cfg.errors.empty() && cv.overall_pass ? (cv.capacity_ok ? "PASS" : "PASS_WITH_CAPACITY_WARNINGS") : "FAIL") << "\n";
}

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <input_case.csv> [more_case.csv ...]\n";
        cerr << "This Step-8 version parses, places blocks, generates channels, routes all nonzero connections through channels only, writes a .cfg file, then validates the generated .cfg format/geometry/routing.\n";
        return 1;
    }

    bool anyFail = false;
    for (int i = 1; i < argc; ++i) {
        string filename = argv[i];
        InputData data = parseInputCsv(filename);
        ShapeResult shapes = generateLegalBlockShapes(data);
        EdgePlacementResult edgePlacement = placeEdgeBlocksFirst(data, shapes);
        FullPlacementResult fullPlacement = placeAllBlocksAfterEdges(data, shapes, edgePlacement);
        ChannelGenerationResult channels = generateVerticalSliceChannels(data, fullPlacement);
        RoutingGenerationResult routing = routeAllConnectionsThroughChannelsOnly(data, fullPlacement, channels);
        CfgWriteResult cfg = writeCfgFile(filename, data, fullPlacement, channels, routing);
        CfgValidationResult cfgValidation = validateGeneratedCfgFile(cfg.output_path, data);
        printStep7CfgVerification(filename, data, shapes, edgePlacement, fullPlacement, channels, routing, cfg, cfgValidation);
        if (!data.errors.empty() || !shapes.errors.empty() || !edgePlacement.errors.empty() || !fullPlacement.errors.empty() || !channels.errors.empty() || !routing.errors.empty() || !cfg.errors.empty() || !cfg.written || !cfgValidation.overall_pass) anyFail = true;
    }
    return anyFail ? 2 : 0;
}
