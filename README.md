# Rational Approximation

A tool to find the rational approximation of decimal numbers and fractions, 
giving a maximum size (in digits) of nominator and denominator.

#### Examples of calculations

(Let us limit the nominator and denominator to three digits).

Input         | Rational Approximation
:---          | :--- 
0.12345		  | 119 / 964
1234 / 5678	  |	153 / 704

The program support repeating (recurring) decimals using “(&#xA0;)”, 
as well as exponents:

Input         | Rational Approximation
:---          | :--- 
0.(3)         | 1 / 3
0.12(345)     | 10 / 81
0.8(9)        | 9 / 10
1.234e-4      | 617 / 5

If the input number is too large (not in [-1...+1]), then the result
may include an exponent, for example:

Input         | Rational Approximation
:---          | :--- 
123.45		  | 458e+2 / 371
1234567/89	  |	799e+4 / 576

#### A view of the program

![Screenshot](Screenshot1.png)

<br/>
<br/>
<br/>
