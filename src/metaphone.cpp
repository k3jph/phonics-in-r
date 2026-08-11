// Copyright (c) 2015-2019, James P. Howard, II <jh@jameshoward.us>
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//
//     Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in
//     the documentation and/or other materials provided with the
//     distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

//' @useDynLib phonics
//' @importFrom Rcpp evalCpp
#include <Rcpp.h>
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <string>

namespace {

constexpr char MISSING_CHAR = '\0';
constexpr R_xlen_t INTERRUPT_INTERVAL = 10000;

bool is_ascii_letter(char value) {
    return (value >= 'A' && value <= 'Z') ||
           (value >= 'a' && value <= 'z');
}

bool is_space(char value) {
    return std::isspace(static_cast<unsigned char>(value)) != 0;
}

std::string trim_and_uppercase(std::string value) {
    value.erase(
        std::find_if_not(value.rbegin(), value.rend(), is_space).base(),
        value.end()
    );
    value.erase(
        value.begin(),
        std::find_if_not(value.begin(), value.end(), is_space)
    );

    std::transform(
        value.begin(),
        value.end(),
        value.begin(),
        [](char character) {
            return static_cast<char>(
                std::toupper(static_cast<unsigned char>(character))
            );
        }
    );

    return value;
}

bool contains(const std::string& characters, char value) {
    return value != MISSING_CHAR &&
           characters.find(value) != std::string::npos;
}

char char_at(const std::string& word, std::ptrdiff_t position) {
    if(position < 0)
        return MISSING_CHAR;

    const std::size_t index = static_cast<std::size_t>(position);
    if(index >= word.size())
        return MISSING_CHAR;

    return word[index];
}

bool matches_at(
    const std::string& word,
    std::ptrdiff_t position,
    const std::string& expected
) {
    if(position < 0)
        return false;

    const std::size_t index = static_cast<std::size_t>(position);
    if(index > word.size() || expected.size() > word.size() - index)
        return false;

    return word.compare(index, expected.size(), expected) == 0;
}

bool code_has_capacity(const std::string& code, int maxCodeLen) {
    // Preserve the historical behavior for negative values until the API
    // contract for maxCodeLen is addressed separately.
    return maxCodeLen < 0 ||
           code.size() < static_cast<std::size_t>(maxCodeLen);
}

std::string metaphone_single(std::string x, int maxCodeLen, bool traditional) {
    const std::string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const std::string soft = "EIY";
    const std::string vowels = "AEIOU";

    const std::string word = trim_and_uppercase(x);
    std::string meta;
    char lastChar = MISSING_CHAR;

    /*
     * First, we will handle a few special cases.  The Metaphone of the
     * null string is, itself, the null string.  The Metaphone of a
     * single character is itself, capitalized, as appropriate.
     */
    std::ptrdiff_t position = 0;
    while(char_at(word, position) != MISSING_CHAR &&
          !is_ascii_letter(char_at(word, position))) {
        ++position;
    }

    if(char_at(word, position) == MISSING_CHAR)
        return "";
    if(word.length() == 1)
        return(word);

    switch (char_at(word, position)) {
    case 'A':
        meta += char_at(word, position + 1) == 'E' ?
            char_at(word, position + 1) : char_at(word, position);
        position += 1;
        break;
    case 'G':
    case 'K':
    case 'P':
        if (char_at(word, position + 1) == 'N') {
            meta += char_at(word, position + 1);
            position += 2;
        }
        break;
    case 'W':
        if (char_at(word, position + 1) == 'R') {
            meta += char_at(word, position + 1);
            position += 2;
        } else if (char_at(word, position + 1) == 'H' ||
                   contains(vowels, char_at(word, position + 1))) {
            meta += 'W';
            position += 2;
        }
        break;
    case 'X':
        meta += 'S';
        position += 1;
        break;
    case 'E':
    case 'I':
    case 'O':
    case 'U':
        meta += char_at(word, position);
        ++position;
        break;
    }

    while(code_has_capacity(meta, maxCodeLen) &&
          char_at(word, position) != MISSING_CHAR) {
        const char currentChar = char_at(word, position);
        const char nextChar = char_at(word, position + 1);
        const char nextNextChar = char_at(word, position + 2);

        if(currentChar != 'C' && lastChar == currentChar)
            ++position;
        else {
            switch(currentChar) {
            case 'B':
                if (lastChar != 'M')
                    meta += currentChar;
                break;
            case 'C':
                if (contains(soft, nextChar)) {
                    if (nextChar == 'I' && nextNextChar == 'A') {
                        meta += 'X';
                    } else if (lastChar != 'S') {
                        meta += 'S';
                    }
                } else if (nextChar == 'H') {
                    meta += !traditional &&
                        (nextNextChar == 'R' || lastChar == 'S') ? 'K' : 'X';
                    ++position;
                } else {
                    meta += 'K';
                }
                break;
            case 'D':
                if (nextChar == 'G' && contains(soft, nextNextChar)) {
                    meta += 'J';
                    ++position;
                } else {
                    meta += 'T';
                }
                break;
            case 'G':
                if (nextChar == 'H') {
                    if(!(contains("BDH", char_at(word, position - 3)) ||
                         char_at(word, position - 4) == 'H')) {
                        meta += 'F';
                        ++position;
                    }
                } else if(nextChar == 'N') {
                    if (contains(alpha, nextNextChar) &&
                        !matches_at(word, position + 1, "NED")) {
                        meta += 'K';
                    }
                } else if(contains(soft, nextChar) && lastChar != 'G') {
                    meta += 'J';
                } else {
                    meta += 'K';
                }
                break;
            case 'H':
                if(contains(vowels, nextChar) &&
                   !contains("CGPST", lastChar)) {
                    meta += currentChar;
                }
                break;
            case 'K':
                if (lastChar != 'C') {
                    meta += 'K';
                }
                break;
            case 'P':
                meta += nextChar == 'H' ? 'F' : currentChar;
                break;
            case 'Q':
                meta += 'K';
                break;
            case 'S':
                if(nextChar == 'I' && contains("AO", nextNextChar)) {
                    meta += 'X';
                } else if(nextChar == 'H') {
                    meta += 'X';
                    position += 1;
                } else if(!traditional &&
                          matches_at(word, position + 1, "CHW")) {
                    meta += 'X';
                    position += 2;
                } else {
                    meta += 'S';
                }
                break;
            case 'T':
                if(nextChar == 'I' && contains("AO", nextNextChar)) {
                    meta += 'X';
                } else if(nextChar == 'H') {
                    meta += '0';
                    position += 1;
                } else if(!matches_at(word, position + 1, "CH")) {
                    meta += 'T';
                }
                break;
            case 'V':
                meta += 'F';
                break;
            case 'W':
            case 'Y':
                if(contains(vowels, nextChar))
                    meta += currentChar;
                break;
            case 'X':
                meta += "KS";
                break;
            case 'Z':
                meta += 'S';
                break;
            case 'F':
            case 'J':
            case 'L':
            case 'M':
            case 'N':
            case 'R':
                meta += currentChar;
                break;
            default:
                break;
            }
            // The historical iterator implementation recorded the final
            // character consumed by a multi-character rule (for example,
            // H in GH), not always the character that entered the switch.
            lastChar = char_at(word, position);
            ++position;
        }
    }
    return meta;
}

} // namespace

//[[Rcpp::export]]
Rcpp::CharacterVector metaphone_internal(Rcpp::CharacterVector word, int maxCodeLen = 10) {

    const R_xlen_t input_size = word.size();
    Rcpp::CharacterVector res(input_size);

    for(R_xlen_t i = 0; i < input_size; ++i){
        if((i % INTERRUPT_INTERVAL) == 0){
            Rcpp::checkUserInterrupt();
        }
        if(word[i] == NA_STRING){
            res[i] = NA_STRING;
        } else {
            res[i] = metaphone_single(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
        }
    }

    return res;
}
