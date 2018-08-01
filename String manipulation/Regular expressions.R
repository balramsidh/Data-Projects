################################################
############# Chapter 3 ########################
############ Regular expressions ##################


packages = c("ggplot2","stringr","babynames", "rebus","neiss")

package.check <- lapply(packages, FUN= function(pkg)  {
  if(!require(pkg, character.only = T)) {
    install.packages(pkg, dependencies = T)
    library(pkg, character.only = T)
  }
})




# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")

# Print END
END

# Run me
str_view(x, pattern = START %R% "c")

str_view(x, pattern = "^c")

# Match the strings that start with "co" 
str_view(x, pattern = START %R% "co")

str_view(x, pattern = "^co")
# Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)
str_view(x, pattern = "at$")

# Match the strings that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)
str_view(x, pattern = "^cat$")


x <- c("cat", "coat", "scotland", "tic toc")

# Match any character followed by a "t"
str_view(x, pattern = ANY_CHAR %R% "t")
str_view(x, pattern = ".t")

# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)

# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)

str_view(x, pattern = "..") # notice that reg exp is lazy, only return first match

# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END )

str_view(x, pattern = "^...$" )

# q followed by any character
pattern <- "q" %R% ANY_CHAR

# Test pattern 
str_view(c("Quentin", "Kaliq", "Jacques",  "Jacqes"), pattern)  

# Find names that have the pattern
names_with_q <- str_subset(boy_names, pattern)

length(names_with_q)

# Find part of name that matches pattern
part_with_q <- str_extract(boy_names, pattern)

table(part_with_q)

# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names, pattern)

table(count_of_q)

# Which babies got these names?
with_q <- str_detect(boy_names, pattern)


# What fraction of babies got these names?

mean(with_q)

# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey","Geoffrey")
str_view(boy_names, pattern = whole_names, 
         match = TRUE)

# Match Jeffrey or Geoffrey, another way
common_ending <- or("Je","Geo") %R% "ffrey"
str_view(boy_names, pattern = common_ending, 
         match = TRUE)

# Match with alternate endings
by_parts <- or("Je","Geo") %R% "ff" %R% or("ry","ery","rey","erey")
str_view(boy_names, pattern = by_parts, match = TRUE)

# Match names that start with Cath or Kath
ckath <- START %R% or("Cath","Kath")
str_view(girl_names, pattern = ckath,match = TRUE)

# Create character class containing vowels
vowels <- char_class("aeiouAEIOU")

# Print vowels
vowels

# See vowels in x with str_view()
str_view(x, vowels)

# See vowels in x with str_view_all()
str_view_all(x, vowels)

# Number of vowels in boy_names
num_vowels <- str_count(boy_names, vowels)
mean(num_vowels)

# Proportion of vowels in boy_names
name_length <- str_length(boy_names)
mean(num_vowels/name_length)


x <- c("ow", "ooh", "yeeeah!", "shh")
str_view(x, pattern = zero_or_more(vowels))

# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")

# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class(vowels)

# See names with only vowels
str_view(boy_names, 
         pattern = exactly(vowels), 
         match = TRUE)

# See names with no vowels
str_view(boy_names, 
         pattern = one_or_more(not_vowels), 
         match = TRUE)

# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")

# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")

negated_char_class(vowels)


# See names with only vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(vowels)), 
         match = TRUE)

# See names with no vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(not_vowels)), 
         match = TRUE)

####
# Take a look at ALL digits
str_view_all(contact, pattern = DGT)

# Create a three digit pattern and test
three_digits <- DGT %R% DGT %R% DGT
str_view_all(contact,
             pattern = three_digits)

# Create four digit pattern
four_digits <- DGT %R% DGT %R% DGT %R% DGT

# Create a separator pattern and test
separator <-  char_class("-.() ")
str_view_all(contact,
             pattern = separator)

# Create phone pattern
phone_pattern <- zero_or_more(OPEN_PAREN) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits %R% 
  zero_or_more(separator) %R%
  four_digits

# Test pattern           
str_view(contact, phone_pattern)

# Extract phone numbers
str_extract(contact, pattern = phone_pattern)

# Extract ALL phone numbers
str_extract_all(contact, pattern = phone_pattern)

# Look for two digits
str_view(narratives, pattern = DGT %R% DGT )



# Pattern to match one or two digits
age <- one_or_more(DGT)
str_view(narratives, pattern = age)

# Pattern to match units 
unit <- optional(SPC) %R% or("YO","YR","MO")

# Test pattern with age then units
str_view(narratives, pattern = age %R% unit)

# Pattern to match gender
gender <- optional(SPC) %R% char_class("MF")

# Test pattern with age then units then gender
str_view(narratives, pattern = age %R% unit %R% gender)



# Extract age_gender, take a look
age_gender <- str_extract(narratives, pattern = age %R% unit %R% gender)
age_gender

# age_gender, age, gender, unit are pre-defined
ls.str()

# Extract age and make numeric
ages_numeric <- as.numeric(str_extract(age_gender, age))

# Replace age and units with ""
genders <- str_replace(age_gender, 
                       pattern = age %R% unit, 
                       replacement = "")


# Replace extra spaces
genders_clean <- str_replace_all(genders, 
                                 pattern = SPC, 
                                 replacement = "")



# Extract units 
time_units <- str_extract(age_gender, unit)

time_units
time_units_clean

# Extract first word character
time_units_clean <- str_extract(time_units, WRD)

# Turn ages in months to years
ages_years <- ifelse(time_units_clean == "Y", ages_numeric, ages_numeric/12)

