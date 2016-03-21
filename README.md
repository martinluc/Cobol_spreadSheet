# Cobol_spreadSheet
Manage two spreadsheets (three and one dimension) using another program.

Main : Cob_CallTables_1of2
Program called : Cob_CallTables_2of2

Details :
********

The main program creates a first spreadsheet (three dimensions : a, b and c for exemple). Each square contains the sum of its indexes
a[i]b[j]c[k] = i+j+k for exemple a[1]b[1]c[1] = 3. Initialized in the main.

A second spreadsheet (one dimension for exemple S, from 1 to 30) is created containing the previous sum as index and the occurences of that sum. S[3] = 1 the number '3' appears only one time. Initialized in the called program.

The program returns the two spreadsheets and the most occured square(s).

