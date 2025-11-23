#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <chrono>
using namespace std;

using u32 = uint32_t;
using u64 = uint64_t;
using u128 = __uint128_t;
using Big = vector<u32>;

class BigNum
{
private:
    Big digits;

    void trim()
    {
        while (digits.size() > 1 && digits.back() == 0)
        {
            digits.pop_back();
        }
        if (digits.empty())
        {
            digits.push_back(0);
        }
    }

    BigNum shift_left(int bits) const
    {
        if (bits == 0)
            return *this;
        if (isZero())
            return BigNum(0);

        int wordShift = bits / 32;
        int bitShift = bits % 32;

        BigNum r;
        r.digits.resize(digits.size() + wordShift + 1, 0);

        u32 carry = 0;
        for (size_t i = 0; i < digits.size(); ++i)
        {
            u64 cur = ((u64)digits[i] << bitShift) | carry;
            r.digits[i + wordShift] = (u32)cur;
            carry = (u32)(cur >> 32);
        }
        r.digits[digits.size() + wordShift] = carry;
        r.trim();
        return r;
    }

    BigNum shift_right_1() const
    {
        BigNum r = *this;
        u32 carry = 0;
        for (int i = (int)r.digits.size() - 1; i >= 0; i--)
        {
            u64 cur = ((u64)carry << 32) | r.digits[i];
            r.digits[i] = (u32)(cur >> 1);
            carry = (u32)(cur & 1);
        }
        r.trim();
        return r;
    }

    int find_msb() const
    {
        if (isZero())
            return -1;
        int lastLimb = digits.size() - 1;
        u32 msb_limb = digits[lastLimb];
        int bit_pos = 31;
        while (bit_pos > 0 && ((msb_limb >> bit_pos) & 1) == 0)
        {
            bit_pos--;
        }
        return lastLimb * 32 + bit_pos;
    }

    pair<BigNum, BigNum> divMod(const BigNum &divisor) const
    {
        if (divisor.isZero())
            throw runtime_error("Division by zero");

        BigNum quotient;
        BigNum remainder;
        quotient.digits.resize(digits.size(), 0);

        for (int i = (int)digits.size() - 1; i >= 0; i--)
        {

            remainder = remainder.shift_left(32);
            remainder = remainder + BigNum((u64)digits[i]);

            u32 quotient_digit = 0;
            u32 left = 0;
            u32 right = 0xFFFFFFFF;

            while (left <= right)
            {
                u32 mid = left + (right - left) / 2;

                if (mid == 0)
                {
                    if (left == 0)
                        left++;
                    else
                        break;
                }

                BigNum test_product = divisor * BigNum((u64)mid);

                if (test_product.cmp(remainder) <= 0)
                {
                    quotient_digit = mid;
                    left = mid + 1;
                }
                else
                {
                    right = mid - 1;
                }
            }

            quotient.digits[i] = quotient_digit;

            remainder = remainder - divisor * BigNum((u64)quotient_digit);
        }

        quotient.trim();
        return {quotient, remainder};
    }

public:
    BigNum() { digits = {0}; }

    BigNum(u64 val)
    {
        digits.clear();
        if (val == 0)
        {
            digits.push_back(0);
            return;
        }
        digits.push_back((u32)val);
        if (val >> 32)
        {
            digits.push_back((u32)(val >> 32));
        }
    }

    BigNum(string hexStr)
    {
        fromReversedHex(hexStr);
    }

    void fromReversedHex(string hexStr)
    {
        string s;
        for (char c : hexStr)
            if (!isspace((unsigned char)c))
                s += c;

        size_t pos = s.find_first_not_of('0');
        if (pos == string::npos)
        {
            digits = {0};
            return;
        }
        s = s.substr(pos);

        reverse(s.begin(), s.end());

        if (s.empty())
        {
            digits = {0};
            return;
        }

        if (s.size() % 8 != 0)
            s = string(8 - s.size() % 8, '0') + s;

        digits.clear();

        for (int i = (int)s.size() - 8; i >= 0; i -= 8)
        {
            string limbHex = s.substr(i, 8);
            u32 limbVal = 0;
            for (char c : limbHex)
            {
                limbVal <<= 4;
                if (c >= '0' && c <= '9')
                    limbVal |= (c - '0');
                else if (c >= 'a' && c <= 'f')
                    limbVal |= (c - 'a' + 10);
                else if (c >= 'A' && c <= 'F')
                    limbVal |= (c - 'A' + 10);
            }
            digits.push_back(limbVal);
        }
        trim();
    }

    string toReversedHex() const
    {
        if (isZero())
            return "0";

        static const char HEX[] = "0123456789ABCDEF";
        string out;

        for (int i = (int)digits.size() - 1; i >= 0; i--)
        {
            u32 b = digits[i];
            string limbHex;
            for (int j = 0; j < 8; j++)
            {
                limbHex += HEX[(b >> (4 * (7 - j))) & 0xF];
            }
            out += limbHex;
        }

        size_t pos = out.find_first_not_of('0');
        if (pos == string::npos)
            return "0";
        return out.substr(pos);
    }

    bool isZero() const
    {
        return digits.size() == 1 && digits[0] == 0;
    }

    bool isOdd() const
    {
        return digits.empty() ? false : (digits[0] & 1);
    }

    BigNum div2() const
    {
        return shift_right_1();
    }

    int cmp(const BigNum &b) const
    {
        if (digits.size() != b.digits.size())
            return digits.size() < b.digits.size() ? -1 : 1;
        for (int i = (int)digits.size() - 1; i >= 0; i--)
            if (digits[i] != b.digits[i])
                return digits[i] < b.digits[i] ? -1 : 1;
        return 0;
    }

    BigNum operator+(const BigNum &b) const
    {
        BigNum r;
        r.digits.assign(max(digits.size(), b.digits.size()), 0);
        u64 carry = 0;
        for (size_t i = 0; i < r.digits.size() || carry; i++)
        {
            if (i == r.digits.size())
                r.digits.push_back(0);

            u64 sum = carry;
            if (i < digits.size())
                sum += digits[i];
            if (i < b.digits.size())
                sum += b.digits[i];

            r.digits[i] = (u32)sum;
            carry = sum >> 32;
        }
        r.trim();
        return r;
    }

    BigNum operator-(const BigNum &b) const
    {
        if (cmp(b) < 0)
            return BigNum(0);
        BigNum r = *this;
        u64 borrow = 0;
        for (size_t i = 0; i < b.digits.size() || borrow; i++)
        {
            u64 diff = ((u64)1 << 32) + r.digits[i] - (i < b.digits.size() ? b.digits[i] : 0) - borrow;
            r.digits[i] = (u32)diff;
            borrow = (diff >> 32) ? 0 : 1;
        }
        r.trim();
        return r;
    }

    BigNum operator*(const BigNum &b) const
    {
        BigNum result;
        int n = digits.size();
        int m = b.digits.size();
        result.digits.assign(n + m, 0);

        for (size_t i = 0; i < n; i++)
        {
            u64 carry = 0;

            for (size_t j = 0; j < m; j++)
            {
                u64 product = (u64)digits[i] * (u64)b.digits[j];
                u64 sum = (u64)result.digits[i + j] + product + carry;

                result.digits[i + j] = (u32)sum;
                carry = sum >> 32;
            }

            if (carry > 0)
            {
                result.digits[i + m] += (u32)carry;
            }
        }

        result.trim();
        return result;
    }

    BigNum operator/(const BigNum &b) const
    {
        return divMod(b).first;
    }

    BigNum operator%(const BigNum &b) const
    {
        return divMod(b).second;
    }

    static BigNum modPow(const BigNum &base, BigNum exp, const BigNum &mod)
    {
        BigNum result(1);
        BigNum b = base % mod;
        BigNum e = exp;

        while (!e.isZero())
        {
            if (e.isOdd())
                result = (result * b) % mod;
            b = (b * b) % mod;
            e = e.div2();
        }
        return result;
    }

    /**
     * Tính nghịch đảo modular của a mod m bằng Extended Euclidean Algorithm
     * Trả về x sao cho (a * x) mod m = 1
     */
    static BigNum modInverse(const BigNum &a, const BigNum &m)
    {
        BigNum m0 = m;
        BigNum x0(0), x1(1);
        BigNum temp_a = a % m;

        if (m.cmp(BigNum(1)) == 0)
            return BigNum(0);

        while (temp_a.cmp(BigNum(1)) > 0)
        {
            BigNum q = temp_a / m0;
            BigNum t = m0;

            m0 = temp_a % m0;
            temp_a = t;

            t = x0;
            x0 = x1 - q * x0;
            x1 = t;
        }

        if (x1.cmp(BigNum(0)) < 0)
            x1 = x1 + m;

        return x1;
    }
};

/**
 * Ký một văn bản bằng ElGamal
 * @param m: Văn bản cần ký (hash của message)
 * @param privateKey: Khóa riêng a
 * @param g: Căn nguyên thủy
 * @param p: Số nguyên tố
 * @param k: Số ngẫu nhiên nguyên tố cùng nhau với p-1
 * @return: Cặp chữ ký (r, s) với r = g^k mod p, s = (m - a*r) * k^(-1) mod (p-1)
 * 
 * Các bước thực hiện:
 * 1. Tính r = g^k mod p (sử dụng modPow)
 * 2. Tính p_minus_1 = p - 1
 * 3. Tính k_inv = k^(-1) mod (p-1) (sử dụng modInverse)
 * 4. Tính a*r mod (p-1)
 * 5. Tính (m - a*r) mod (p-1), xử lý trường hợp âm
 * 6. Tính s = (m - a*r) * k_inv mod (p-1)
 * 7. Trả về {r, s}
 */
pair<BigNum, BigNum> elgamalSign(const BigNum& m, const BigNum& privateKey,
                                 const BigNum& g, const BigNum& p, const BigNum& k)
{
    // TODO: Implement ElGamal signature
    // Step 1: r = g^k mod p
    // Step 2: p_minus_1 = p - 1
    // Step 3: k_inv = modInverse(k, p_minus_1)
    // Step 4: ar = (privateKey * r) mod p_minus_1
    // Step 5: m_mod = m mod p_minus_1
    // Step 6: diff = (m_mod - ar) mod p_minus_1 (handle negative)
    // Step 7: s = (diff * k_inv) mod p_minus_1
    // return {r, s};
}

/**
 * Xác thực chữ ký ElGamal
 * @param m: Văn bản gốc (hash của message)
 * @param r: Phần thứ nhất của chữ ký
 * @param s: Phần thứ hai của chữ ký
 * @param publicKey: Khóa công khai h = g^a mod p
 * @param g: Căn nguyên thủy
 * @param p: Số nguyên tố
 * @return: true nếu chữ ký hợp lệ (g^m ≡ h^r * r^s (mod p))
 * 
 * Các bước thực hiện:
 * 1. Kiểm tra 0 < r < p, nếu không thỏa trả về false
 * 2. Tính left = g^m mod p (sử dụng modPow)
 * 3. Tính h^r mod p (sử dụng modPow)
 * 4. Tính r^s mod p (sử dụng modPow)
 * 5. Tính right = (h^r * r^s) mod p
 * 6. So sánh left == right, trả về kết quả
 * 
 * Giải thích:
 * - Từ s = (m - a*r) * k^(-1) mod (p-1)
 * - Suy ra: m = a*r + k*s mod (p-1)
 * - Do đó: g^m = g^(a*r + k*s) = (g^a)^r * (g^k)^s = h^r * r^s mod p
 */
bool elgamalVerify(const BigNum& m, const BigNum& r, const BigNum& s,
                   const BigNum& publicKey, const BigNum& g, const BigNum& p)
{
    // TODO: Implement ElGamal signature verification
    // Step 1: Check 0 < r < p
    // Step 2: left = g^m mod p
    // Step 3: hr = publicKey^r mod p
    // Step 4: rs = r^s mod p
    // Step 5: right = (hr * rs) mod p
    // Step 6: return left == right
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    string input_path = argv[1];
    string output_path = argv[2];

    ifstream inputFile(input_path);
    if (!inputFile.is_open())
    {
        cerr << "Error: Cannot open input file " << input_path << endl;
        return 1;
    }

    string p_hex, g_hex, a_hex, m_hex, k_hex, r_hex;
    inputFile >> p_hex >> g_hex >> a_hex >> m_hex >> k_hex >> r_hex;
    inputFile.close();

    BigNum p(p_hex);
    BigNum g(g_hex);
    BigNum a(a_hex);      // Khóa riêng
    BigNum m(m_hex);      // Văn bản cần ký
    BigNum k(k_hex);      // Số ngẫu nhiên
    BigNum r_verify(r_hex); // Giá trị r để xác thực (nếu có)
    
    // Tính khóa công khai h = g^a mod p
    BigNum h = BigNum::modPow(g, a, p);
    
    // Ký văn bản
    auto [r, s] = elgamalSign(m, a, g, p, k);
    
    // Xác thực chữ ký
    bool isValid = elgamalVerify(m, r, s, h, g, p);

    ofstream outputFile(output_path);
    if (!outputFile.is_open())
    {
        cerr << "Error: Cannot open output file " << output_path << endl;
        return 1;
    }

    // Xuất kết quả xác thực
    outputFile << (isValid ? "1" : "0");
    outputFile.close();

    return 0;
}
