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
 * Mã hóa một số nguyên bằng ElGamal
 * @param m: Bản rõ
 * @param publicKey: Khóa công khai h = g^a mod p
 * @param g: Căn nguyên thủy
 * @param p: Số nguyên tố
 * @param k: Số ngẫu nhiên (ephemeral key)
 * @return: Cặp (c1, c2) = (g^k mod p, m * h^k mod p)
 * 
 * Các bước thực hiện:
 * 1. Tính c1 = g^k mod p (sử dụng modPow)
 * 2. Tính h^k mod p (sử dụng modPow)
 * 3. Tính c2 = (m * h^k) mod p
 * 4. Trả về cặp {c1, c2}
 */
pair<BigNum, BigNum> elgamalEncrypt(const BigNum& m, const BigNum& publicKey, 
                                     const BigNum& g, const BigNum& p, const BigNum& k)
{
    // TODO: Implement ElGamal encryption
    // Step 1: c1 = g^k mod p
    // Step 2: hk = publicKey^k mod p
    // Step 3: c2 = (m * hk) mod p
    // return {c1, c2};
}

/**
 * Giải mã bản mã ElGamal
 * @param c1: Phần thứ nhất của bản mã
 * @param c2: Phần thứ hai của bản mã
 * @param privateKey: Khóa riêng a
 * @param p: Số nguyên tố
 * @return: Bản rõ m = c2 * (c1^a)^(-1) mod p
 * 
 * Các bước thực hiện:
 * 1. Tính s = c1^a mod p (sử dụng modPow)
 * 2. Tính s_inv = s^(-1) mod p (sử dụng modInverse)
 * 3. Tính m = (c2 * s_inv) mod p
 * 4. Trả về m
 * 
 * Giải thích: 
 * - c1 = g^k mod p, c2 = m * h^k mod p
 * - s = c1^a = (g^k)^a = g^(ka) = h^k mod p
 * - m = c2 * s^(-1) = m * h^k * (h^k)^(-1) = m mod p
 */
BigNum elgamalDecrypt(const BigNum& c1, const BigNum& c2, 
                      const BigNum& privateKey, const BigNum& p)
{
    // TODO: Implement ElGamal decryption
    // Step 1: s = c1^privateKey mod p
    // Step 2: s_inv = modInverse(s, p)
    // Step 3: m = (c2 * s_inv) mod p
    // return m;
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

    string p_hex, g_hex, a_hex, m_hex, k_hex;
    inputFile >> p_hex >> g_hex >> a_hex >> m_hex >> k_hex;
    inputFile.close();

    BigNum p(p_hex);
    BigNum g(g_hex);
    BigNum a(a_hex);      // Khóa riêng
    BigNum m(m_hex);      // Bản rõ
    BigNum k(k_hex);      // Số ngẫu nhiên
    
    // Tính khóa công khai h = g^a mod p
    BigNum h = BigNum::modPow(g, a, p);
    
    // Mã hóa
    auto [c1, c2] = elgamalEncrypt(m, h, g, p, k);
    
    // Giải mã để kiểm tra
    BigNum decrypted = elgamalDecrypt(c1, c2, a, p);

    ofstream outputFile(output_path);
    if (!outputFile.is_open())
    {
        cerr << "Error: Cannot open output file " << output_path << endl;
        return 1;
    }

    // Xuất bản rõ sau khi giải mã
    outputFile << decrypted.toReversedHex();
    outputFile.close();

    return 0;
}