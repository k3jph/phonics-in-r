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

#include <Rcpp.h>
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <string>

namespace {

constexpr R_xlen_t INTERRUPT_INTERVAL = 10000;

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

bool is_ascii_upper(char value) {
    return value >= 'A' && value <= 'Z';
}

int alphabet_index(char value) {
    if(!is_ascii_upper(value))
        return -1;

    return static_cast<int>(value - 'A');
}

std::size_t code_length(int maxCodeLen) {
    // Preserve the existing negative-value behavior pending an API decision.
    return static_cast<std::size_t>(maxCodeLen);
}

std::string soundex_single(std::string x, int maxCodeLen) {
    const std::string SOUNDEX = "01230120022455012623010202";
    std::string code;

    x = trim_and_uppercase(x);

    std::string::const_iterator i = std::find_if(
        x.cbegin(),
        x.cend(),
        is_ascii_upper
    );
    if(i == x.end())
        return "";
    if(x.length() == 1) {
        x += "0000";
        x = x.substr(0, code_length(maxCodeLen));
        return(x);
    }

    code = *i;
    char lastCode = SOUNDEX[static_cast<std::size_t>(alphabet_index(*i))];

    for(++i; i != x.end(); ++i) {
        const int currCode = alphabet_index(*i);
        if(currCode < 0)
            break;

        const char nextCode = SOUNDEX[static_cast<std::size_t>(currCode)];
        if(nextCode != '0' && nextCode != lastCode)
            code += (lastCode = nextCode);
        if(nextCode ==  '0' && *i != 'H' && *i != 'W')
            lastCode = '?';
    }

    //  "0"-pad string then truncate
    code += "0000";
    code = code.substr(0, code_length(maxCodeLen));

    return code;
}

std::string refinedSoundex_single(std::string x, int maxCodeLen) {
    const std::string SOUNDEX = "01360240043788015936020505";
    std::string code;

    x = trim_and_uppercase(x);

    std::string::const_iterator i = std::find_if(
        x.cbegin(),
        x.cend(),
        is_ascii_upper
    );
    if(i == x.end())
        return "";
    if(x.length() == 1)
        return(x);

    code = *i;
    char lastCode = SOUNDEX[static_cast<std::size_t>(alphabet_index(*i))];
    code += lastCode;

    for(++i; i != x.end(); ++i) {
        const int currCode = alphabet_index(*i);
        if(currCode < 0)
            break;

        const char nextCode = SOUNDEX[static_cast<std::size_t>(currCode)];
        if(nextCode != lastCode)
            code += (lastCode = nextCode);
    }

    // Do not "0"-pad for refined
    code = code.substr(0, code_length(maxCodeLen));

    return code;
}

} // namespace

//' @useDynLib phonics
//' @importFrom Rcpp evalCpp
//[[Rcpp::export]]
Rcpp::CharacterVector soundex_internal(Rcpp::CharacterVector word, int maxCodeLen = 4) {
    const R_xlen_t input_size = word.size();
    Rcpp::CharacterVector res(input_size);

    for(R_xlen_t i = 0; i < input_size; ++i){
        if((i % INTERRUPT_INTERVAL) == 0){
            Rcpp::checkUserInterrupt();
        }
        if(word[i] == NA_STRING){
            res[i] = NA_STRING;
        } else {
            res[i] = soundex_single(Rcpp::as<std::string>(word[i]), maxCodeLen);
        }
    }

    return res;
}

//' @useDynLib phonics
//' @importFrom Rcpp evalCpp
//[[Rcpp::export]]
Rcpp::CharacterVector refinedSoundex_internal(Rcpp::CharacterVector word, int maxCodeLen = 10) {
    const R_xlen_t input_size = word.size();
    Rcpp::CharacterVector res(input_size);

    for(R_xlen_t i = 0; i < input_size; ++i){
        if((i % INTERRUPT_INTERVAL) == 0){
            Rcpp::checkUserInterrupt();
        }
        if(word[i] == NA_STRING){
            res[i] = NA_STRING;
        } else {
            res[i] = refinedSoundex_single(Rcpp::as<std::string>(word[i]), maxCodeLen);
        }
    }

    return res;
}
