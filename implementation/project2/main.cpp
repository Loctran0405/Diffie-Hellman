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
};

/**
 * Tính khóa công khai từ khóa riêng trong giao thức Diffie-Hellman
 * @param g: Căn nguyên thủy
 * @param privateKey: Khóa riêng
 * @param p: Số nguyên tố
 * @return: Khóa công khai = g^privateKey mod p
 * 
 * Các bước thực hiện:
 * 1. Tính publicKey = g^privateKey mod p bằng hàm modPow
 * 2. Trả về publicKey
 */
BigNum computePublicKey(const BigNum& g, const BigNum& privateKey, const BigNum& p)
{
    // TODO: Implement Diffie-Hellman public key computation
    // return BigNum::modPow(g, privateKey, p);
}

/**
 * Tính khóa chung từ khóa công khai của bên kia trong giao thức Diffie-Hellman
 * @param otherPublicKey: Khóa công khai của bên kia
 * @param myPrivateKey: Khóa riêng của mình
 * @param p: Số nguyên tố
 * @return: Khóa chung = otherPublicKey^myPrivateKey mod p
 * 
 * Các bước thực hiện:
 * 1. Tính sharedKey = otherPublicKey^myPrivateKey mod p bằng hàm modPow
 * 2. Trả về sharedKey
 * 
 * Lưu ý: Alice và Bob sẽ tính được cùng một khóa chung:
 * - Alice tính: B^a mod p = (g^b)^a mod p = g^(ab) mod p
 * - Bob tính: A^b mod p = (g^a)^b mod p = g^(ab) mod p
 */
BigNum computeSharedKey(const BigNum& otherPublicKey, const BigNum& myPrivateKey, const BigNum& p)
{
    // TODO: Implement shared key computation
    // return BigNum::modPow(otherPublicKey, myPrivateKey, p);
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

    string p_hex, g_hex, a_hex, b_hex;
    inputFile >> p_hex >> g_hex >> a_hex >> b_hex;
    inputFile.close();

    BigNum p(p_hex);
    BigNum g(g_hex);
    BigNum a(a_hex);  // Khóa riêng của Alice
    BigNum b(b_hex);  // Khóa riêng của Bob
    
    // Tính khóa công khai
    BigNum A = computePublicKey(g, a, p);  // Alice's public key
    BigNum B = computePublicKey(g, b, p);  // Bob's public key
    
    // Tính khóa chung (cả hai bên sẽ có cùng khóa)
    BigNum sharedKeyAlice = computeSharedKey(B, a, p);
    BigNum sharedKeyBob = computeSharedKey(A, b, p);

    ofstream outputFile(output_path);
    if (!outputFile.is_open())
    {
        cerr << "Error: Cannot open output file " << output_path << endl;
        return 1;
    }

    // Xuất khóa chung (cả hai khóa giống nhau)
    outputFile << sharedKeyAlice.toReversedHex();
    outputFile.close();

    return 0;
}