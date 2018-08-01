################################################
############# Chapter 4 ########################
############ Advanced matching and manipulation ##################


packages = c("ggplot2","stringr","babynames", "rebus")

package.check <- lapply(packages, FUN= function(pkg)  {
  if(!require(pkg, character.only = T)) {
    install.packages(pkg, dependencies = T)
    library(pkg, character.only = T)
  }
})

# Capture part between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
  "@" %R% capture(one_or_more(WRD)) %R% 
  DOT %R% capture(one_or_more(WRD))

# Check match hasn't changed
str_view(hero_contacts, email)

# Pull out match and captures
email_parts <- str_match(hero_contacts, email)

# Print email_parts
email_parts
# Save host
host <- email_parts[,3]
host


# View text containing phone numbers
contact

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
  capture(three_digits) %R% zero_or_more(separator) %R%
  capture(four_digits)

# Pull out the parts with str_match()
phone_numbers <- str_match(contact, phone_pattern)

phone_numbers
# Put them back together
str_c("(",phone_numbers[,2],") ",phone_numbers[,3], "-",phone_numbers[,4])

# narratives has been pre-defined
narratives

# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Pull out from narratives
str_match(narratives, pattern)

# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Check pattern
str_view(narratives, pattern2)

# Pull out pieces
str_match(narratives, pattern2)


str_view("paris in the the rain", 
         SPC %R%
           capture(one_or_more(WRD)) %R%
           SPC %R%
           REF1
           )
str_view(c("hello", "sweet", "kitten"), 
         pattern = capture(LOWER) %R% REF1)

LOWER

# See names with three repeated letters
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1
str_view(boy_names, 
         pattern = repeated_three_times, 
         match = TRUE)

# See names with a pair of repeated letters
pair_of_repeated <- capture(LOWER %R% LOWER) %R% REF1

str_view(boy_names, 
         pattern = pair_of_repeated, 
         match = TRUE)

# See names with a pair that reverses
pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1
str_view(boy_names, 
         pattern = pair_that_reverses, 
         match = TRUE)

# See four letter palindrome names
four_letter_palindrome <- exactly( capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1)
str_view(boy_names, 
         pattern = four_letter_palindrome, 
         match = TRUE)

# See six letter palindrome names
six_letter_palindrome <- exactly( capture(LOWER) %R% capture(LOWER) %R% capture(LOWER) %R% REF3 %R% REF2 %R% REF1)
str_view(boy_names, 
         pattern = six_letter_palindrome, 
         match = TRUE)

# View text containing phone numbers
contact

# Replace digits with "X"
str_replace(contact, pattern = DGT, replacement = "X")

# Replace all digits with "X"
str_replace_all(contact, pattern = DGT, replacement = "X")

# Replace all digits with different symbol
str_replace_all(contact, pattern = DGT, replacement = c("X", ".", "*", "_"))

# Build pattern to match words ending in "ING"
pattern <- capture(one_or_more(UPPER) %R% capture("ING"))
str_view(narratives, pattern)

# Test replacement
str_replace(narratives, pattern, str_c("CARELESSLY", REF1, sep = " "))

# One adverb per narrative
adverbs_10 <- sample(adverbs,10)


# Replace "***ing" with "adverb ***ing"
str_replace(narratives, pattern, str_c(adverbs_10, REF1, sep = " "))


x <- c("\u00e8", "\u0065\u0300")
writeLines(x)
as.hexmode(utf8ToInt(stri_trans_nfd("\u00e8")))
as.hexmode(utf8ToInt(stri_trans_nfc("\u0065\u0300")))

# Names with builtin accents
(tay_son_builtin <- c(
  "Nguy\u1ec5n Nh\u1ea1c", 
  "Nguy\u1ec5n Hu\u1ec7",
  "Nguy\u1ec5n Quang To\u1ea3n"
))

# Convert to separate accents
tay_son_separate <- stri_trans_nfd(tay_son_builtin)

# Verify that the string prints the same
tay_son_separate

# Match all accents
str_view_all(tay_son_separate, UP_DIACRITIC)

x <- c("Adele", "Ad\u00e8le", "Ad\u0065\u0300le")
writeLines(x)

# tay_son_separate has been pre-defined
tay_son_separate

# View all the characters in tay_son_separate
str_view_all(tay_son_separate, ANY_CHAR)

# View all the graphemes in tay_son_separate
str_view_all(tay_son_separate, GRAPHEME)

# Combine the diacritics with their letters
tay_son_builtin <- stri_trans_nfc(tay_son_separate)

# View all the graphemes in tay_son_builtin
str_view_all(tay_son_builtin,GRAPHEME )


whole_word

