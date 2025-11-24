#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cctype>
using namespace std;
class BigNum {
private:
    vector<int> digits;

public:
    BigNum();
    BigNum(long long val);
    BigNum(string hexStr);

    void fromReversedHex(string hexStr);
    string toReversedHex() const;
    int cmp(const BigNum &b) const;
    bool isZero() const;
    bool isOdd() const;
    BigNum div2() const;

    BigNum operator+(const BigNum &b) const;
    BigNum operator-(const BigNum &b) const;
    BigNum operator*(const BigNum &b) const;
    BigNum operator%(const BigNum &b) const;
    BigNum operator/(const BigNum &b) const;
    const vector<int>& getDigits() const { return digits; }
};

BigNum::BigNum() { digits = {0}; }

BigNum::BigNum(long long val) {
    digits.clear();
    if (val == 0) {
        digits.push_back(0);
        return;
    }
    while (val > 0) {
        digits.push_back(val % 256);
        val /= 256;
    }
}

BigNum::BigNum(string hexStr) { fromReversedHex(hexStr); }

void BigNum::fromReversedHex(string hexStr) {
    string s;
    for (char c : hexStr)
        if (!isspace((unsigned char)c))
            s += c;
    reverse(s.begin(), s.end());
    if (s.empty()) { digits = {0}; return; }
    if (s.size() % 2 != 0) s = "0" + s;
    digits.clear();
    for (int i = s.size() - 2; i >= 0; i -= 2) {
        char hi_c = s[i];
        char lo_c = s[i + 1];
        int hi = isdigit(static_cast<unsigned char>(hi_c)) ? (hi_c - '0')
                  : (toupper(static_cast<unsigned char>(hi_c)) - 'A' + 10);
        int lo = isdigit(static_cast<unsigned char>(lo_c)) ? (lo_c - '0')
                  : (toupper(static_cast<unsigned char>(lo_c)) - 'A' + 10);
        digits.push_back(((hi << 4) | lo) & 0xFF);
    }
    while (digits.size() > 1 && digits.back() == 0) digits.pop_back();
}

string BigNum::toReversedHex() const {
    static const char HEX[] = "0123456789ABCDEF";
    if (digits.empty()) return "00";

    string out;
    for (int i = static_cast<int>(digits.size()) - 1; i >= 0; --i) {
        int b = digits[i];
        out += HEX[(b >> 4) & 0xF];
        out += HEX[b & 0xF];
    }
    if (out.empty()) return "00";
    size_t pos = out.find_first_not_of('0');
    if (pos == string::npos) return "00";
    return out.substr(pos);
}

int BigNum::cmp(const BigNum &b) const {
    if (digits.size() != b.digits.size())
        return digits.size() < b.digits.size() ? -1 : 1;
    for (int i = digits.size() - 1; i >= 0; i--)
        if (digits[i] != b.digits[i])
            return digits[i] < b.digits[i] ? -1 : 1;
    return 0;
}

bool BigNum::isZero() const { return digits.size() == 1 && digits[0] == 0; }
bool BigNum::isOdd() const { return digits[0] & 1; }

BigNum BigNum::div2() const {
    BigNum r = *this;
    int carry = 0;
    for (int i = r.digits.size() - 1; i >= 0; i--) {
        int cur = r.digits[i] + carry * 256;
        r.digits[i] = cur / 2;
        carry = cur % 2;
    }
    while (r.digits.size() > 1 && r.digits.back() == 0) r.digits.pop_back();
    return r;
}

BigNum BigNum::operator+(const BigNum &b) const {
    BigNum r;
    r.digits.assign(max(digits.size(), b.digits.size()) + 1, 0);
    int carry = 0;
    for (size_t i = 0; i < r.digits.size(); i++) {
        int sum = carry;
        if (i < digits.size()) sum += digits[i];
        if (i < b.digits.size()) sum += b.digits[i];
        r.digits[i] = sum % 256;
        carry = sum / 256;
    }
    while (r.digits.size() > 1 && r.digits.back() == 0) r.digits.pop_back();
    return r;
}

BigNum BigNum::operator-(const BigNum &b) const {
    BigNum r;
    r.digits.assign(digits.size(), 0);
    int borrow = 0;
    for (size_t i = 0; i < digits.size(); i++) {
        int diff = digits[i] - (i < b.digits.size() ? b.digits[i] : 0) - borrow;
        if (diff < 0) {
            diff += 256;
            borrow = 1;
        } else borrow = 0;
        r.digits[i] = diff;
    }
    while (r.digits.size() > 1 && r.digits.back() == 0) r.digits.pop_back();
    return r;
}

BigNum BigNum::operator*(const BigNum &b) const {
    BigNum r;
    r.digits.assign(digits.size() + b.digits.size(), 0);
    for (size_t i = 0; i < digits.size(); i++) {
        long long carry = 0;
        for (size_t j = 0; j < b.digits.size() || carry; j++) {
            long long cur = r.digits[i + j] +
                (long long)digits[i] * (j < b.digits.size() ? b.digits[j] : 0) + carry;
            r.digits[i + j] = cur % 256;
            carry = cur / 256;
        }
    }
    while (r.digits.size() > 1 && r.digits.back() == 0) r.digits.pop_back();
    return r;
}

BigNum BigNum::operator%(const BigNum &m) const {
    if (m.isZero()) return BigNum(0);
    if (this->cmp(m) < 0) return *this;

    BigNum dividend = *this;
    BigNum divisor = m;
    BigNum current(0);

    for (int i = dividend.digits.size() - 1; i >= 0; i--) {
        current = current * BigNum(256);
        current = current + BigNum(dividend.digits[i]);
        int x = 0, left = 0, right = 255;
        while (left <= right) {
            int mid = (left + right) / 2;
            BigNum t = divisor * BigNum(mid);
            if (t.cmp(current) <= 0) {
                x = mid;
                left = mid + 1;
            } else right = mid - 1;
        }
        current = current - divisor * BigNum(x);
    }
    return current;
}

BigNum BigNum::operator/(const BigNum &b) const {
    if (b.isZero()) throw runtime_error("Division by zero");
    if (this->cmp(b) < 0) return BigNum(0);

    BigNum dividend = *this;
    BigNum divisor = b;
    BigNum quotient;
    quotient.digits.assign(dividend.digits.size(), 0);

    BigNum current;
    for (int i = dividend.digits.size() - 1; i >= 0; i--) {
        current = current * BigNum(256) + BigNum(dividend.digits[i]);

        int left = 0, right = 255, x = 0;
        while (left <= right) {
            int mid = (left + right) / 2;
            BigNum t = divisor * BigNum(mid);
            if (t.cmp(current) <= 0) {
                x = mid;
                left = mid + 1;
            } else right = mid - 1;
        }

        quotient.digits[i] = x;
        current = current - divisor * BigNum(x);
    }
    while (quotient.digits.size() > 1 && quotient.digits.back() == 0)
        quotient.digits.pop_back();
    return quotient;
}

BigNum modPow(BigNum base, BigNum exp, const BigNum &mod) {
    BigNum result(1);
    while (!exp.isZero()) {
        if (exp.isOdd()) result = (result * base) % mod;
        exp = exp.div2();
        base = (base * base) % mod;
    }
    return result;
}

string reverseHex(const string &s) {
    string t = s;
    reverse(t.begin(), t.end());
    return t;
}

int main(int argc, char* argv[]) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 3) {
        cerr << "Usage: program inputFile outputFile\n";
        return 1;
    }

    ifstream fin(argv[1]);
    if (!fin) { cerr << "Cannot open input file\n"; return 1; }

    ofstream fout(argv[2]);
    if (!fout) { cerr << "Cannot open output file\n"; return 1; }

    string pStr, gStr, aStr, bStr;

    getline(fin, pStr);  
    getline(fin, gStr);  
    getline(fin, aStr);  
    getline(fin, bStr); 

    BigNum p(pStr);
    BigNum g(gStr);
    BigNum a(aStr);
    BigNum b(bStr);

    cout << "Input values:\n";
    cout << "p = " << p.toReversedHex() << "\n";
    cout << "g = " << g.toReversedHex() << "\n";
    cout << "a = " << a.toReversedHex() << "\n";
    cout << "b = " << b.toReversedHex() << "\n\n";

    BigNum A = modPow(g, a, p); 
    BigNum B = modPow(g, b, p); 
    BigNum K = modPow(A, b, p); 

    string Aout = reverseHex(A.toReversedHex());
    string Bout = reverseHex(B.toReversedHex());
    string Kout = reverseHex(K.toReversedHex());

    cout << "Output values:\n";
    cout << "A = " << Aout << "\n";
    cout << "B = " << Bout << "\n";
    cout << "K = " << Kout << "\n\n";

    fout << Aout << "\n";
    fout << Bout << "\n";
    fout << Kout << "\n";
    
    return 0;
}

