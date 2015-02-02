class Solution:
    # @return a string
    def find(self, needle, haystack):
        for i, hay in enumerate(haystack):
            if needle == hay:
                return i
        return -1
        
    def decimals(self, numerator, denominator, has_seen, prefix):
        if numerator == 0:
            return prefix
        idx = self.find(numerator, has_seen)
        if idx >= 0:
            return prefix[:idx] + "(" + prefix[idx:] + ")"
        newPrefix = prefix + str(numerator * 10 / denominator)
        has_seen.append(numerator)
        return self.decimals(numerator * 10 % denominator, denominator, has_seen, newPrefix) 
        
    def fractionToDecimal(self, numerator, denominator):
        neg = numerator < 0 and denominator > 0 or numerator > 0 and denominator < 0
        int_part= abs(numerator) / abs(denominator)
        remains = abs(numerator) % abs(denominator)
        result = str(int_part)
        if neg:
            result = "-" + result
        if remains > 0:
            result += "." + self.decimals(remains, abs(denominator), [], "")
        return result
