paste                   package:base                   R Documentation

_C_o_n_c_a_t_e_n_a_t_e _S_t_r_i_n_g_s

_D_e_s_c_r_i_p_t_i_o_n:

     Concatenate vectors after converting to character.

_U_s_a_g_e:

     paste (..., sep = " ", collapse = NULL, recycle0 = FALSE)
     paste0(...,            collapse = NULL, recycle0 = FALSE)
     
_A_r_g_u_m_e_n_t_s:

     ...: one or more R objects, to be converted to character vectors.

     sep: a character string to separate the terms.  Not
          ‘NA_character_’.

collapse: an optional character string to separate the results.  Not
          ‘NA_character_’.

recycle0: ‘logical’ indicating if zero-length character arguments
          should lead to the zero-length ‘character(0)’ after the
          ‘sep’-phase (which turns into ‘""’ in the ‘collapse’-phase,
          i.e., when ‘collapse’ is not ‘NULL’).

_D_e_t_a_i_l_s:

     ‘paste’ converts its arguments (_via_ ‘as.character’) to character
     strings, and concatenates them (separating them by the string
     given by ‘sep’).  If the arguments are vectors, they are
     concatenated term-by-term to give a character vector result.
     Vector arguments are recycled as needed, with zero-length
     arguments being recycled to ‘""’ only if ‘recycle0’ is not true
     _or_ ‘collapse’ is not ‘NULL’.

     Note that ‘paste()’ coerces ‘NA_character_’, the character missing
     value, to ‘"NA"’ which may seem undesirable, e.g., when pasting
     two character vectors, or very desirable, e.g. in ‘paste("the
     value of p is ", p)’.

     ‘paste0(..., collapse)’ is equivalent to ‘paste(..., sep = "",
     collapse)’, slightly more efficiently.

     If a value is specified for ‘collapse’, the values in the result
     are then concatenated into a single string, with the elements
     being separated by the value of ‘collapse’.

_V_a_l_u_e:

     A character vector of the concatenated values.  This will be of
     length zero if all the objects are, unless ‘collapse’ is non-NULL,
     in which case it is ‘""’ (a single empty string).

     If any input into an element of the result is in UTF-8 (and none
     are declared with encoding ‘"bytes"’, see ‘Encoding’), that
     element will be in UTF-8, otherwise in the current encoding in
     which case the encoding of the element is declared if the current
     locale is either Latin-1 or UTF-8, at least one of the
     corresponding inputs (including separators) had a declared
     encoding and all inputs were either ASCII or declared.

     If an input into an element is declared with encoding ‘"bytes"’,
     no translation will be done of any of the elements and the
     resulting element will have encoding ‘"bytes"’.  If ‘collapse’ is
     non-NULL, this applies also to the second, collapsing, phase, but
     some translation may have been done in pasting object together in
     the first phase.

_R_e_f_e_r_e_n_c_e_s:

     Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) _The New S
     Language_.  Wadsworth & Brooks/Cole.

_S_e_e _A_l_s_o:

     ‘toString’ typically calls ‘paste(*, collapse=", ")’.  String
     manipulation with ‘as.character’, ‘substr’, ‘nchar’, ‘strsplit’;
     further, ‘cat’ which concatenates and writes to a file, and
     ‘sprintf’ for C like string construction.

     ‘plotmath’ for the use of ‘paste’ in plot annotation.

_E_x_a_m_p_l_e_s:

     ## When passing a single vector, paste0 and paste work like as.character.
     paste0(1:12)
     paste(1:12)        # same
     as.character(1:12) # same
     
     ## If you pass several vectors to paste0, they are concatenated in a
     ## vectorized way.
     (nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
     
     ## paste works the same, but separates each input with a space.
     ## Notice that the recycling rules make every input as long as the longest input.
     paste(month.abb, "is the", nth, "month of the year.")
     paste(month.abb, letters)
     
     ## You can change the separator by passing a sep argument
     ## which can be multiple characters.
     paste(month.abb, "is the", nth, "month of the year.", sep = "_*_")
     
     ## To collapse the output into a single string, pass a collapse argument.
     paste0(nth, collapse = ", ")
     
     ## For inputs of length 1, use the sep argument rather than collapse
     paste("1st", "2nd", "3rd", collapse = ", ") # probably not what you wanted
     paste("1st", "2nd", "3rd", sep = ", ")
     
     ## You can combine the sep and collapse arguments together.
     paste(month.abb, nth, sep = ": ", collapse = "; ")
     
     ## Using paste() in combination with strwrap() can be useful
     ## for dealing with long strings.
     (title <- paste(strwrap(
         "Stopping distance of cars (ft) vs. speed (mph) from Ezekiel (1930)",
         width = 30), collapse = "\n"))
     plot(dist ~ speed, cars, main = title)
     
     ## 'recycle0 = TRUE' allows more vectorized behaviour, i.e. zero-length recycling :
     valid <- FALSE
     val <- pi
     paste("The value is", val[valid], "-- not so good!")
     paste("The value is", val[valid], "-- good: empty!", recycle0=TRUE) # -> character(0)
     ## When  'collapse = <string>',  the result is a length-1 string :
     paste("foo", {}, "bar", collapse="|")                  # |-->  "foo  bar"
     paste("foo", {}, "bar", collapse="|", recycle0 = TRUE) # |-->  ""
     ## all empty args
     paste(    collapse="|")                  # |-->  ""  as do all these:
     paste(    collapse="|", recycle0 = TRUE)
     paste({}, collapse="|")
     paste({}, collapse="|", recycle0 = TRUE)
     

