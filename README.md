# Crossword puzzle solver and creator

Use function solveWordSearch which takes as parameters [ String ] and WordSearchGrid where [ String ] is an array of strings 
representing the words that you want in the wordsearch and WordSearchGrid represents an array of the characters making up the grid.

For example, if you want to solve a wordsearch, type solveWordSearch [ "HASKELL","STRING","STACK","MAIN","METHOD"] [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ]

The output will be an array containing each word with its starting coordinate and what direction it goes.


Use function createWordSearch which takes as parameters [ String ] and Double where [ String ] is an array of strings 
representing the words that you want in the wordsearch and double represents the density of the grid.

For example, if you want to create a wordsearch, type createWordSearch [ "HASKELL","STRING","STACK","MAIN","METHOD"] 0.5

The output will be a WordSearchGrid represents an array of the characters making up the grid like the parameter used in solveWordSearch.

The below are example grids in array form and example word list in array form:

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
