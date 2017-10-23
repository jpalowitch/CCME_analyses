# a script to modify the mutual3 code to make it return a num instead of print
mut3lines <- readLines("methodFiles/mutual3/mutual3.cpp")
mut3lines[12] <- "  if (argc<4) {"
mut3lines[16] <- '		cerr<<"Usage: ./mutual file1 file2 [-rn]"<<endl;'
mut3lines[22] <- '  if (argc == 4 && argv[3] != "-rn") {'
mut3lines[23] <- '    cerr<<"Allowed flag is -rn, for "return numeric""<<endl;'
mut3lines[24] <- '  }'
mut3lines[66] <- '  double mutscore = mutual3(one, two);'
mut3lines[67] <- '//cout<<"mutual3:\t"<<mutual3(one, two)<<endl;'
mut3lines[68] <- '  if (argc == 4) {'
mut3lines[69] <- '    return mutscore;'
mut3lines[70] <- '  } else {'
mut3lines[71] <- '    cout<<"mutual3:\t"<<mutual3(one, two)<<endl;'
mut3lines[72] <- '    return 0;'
mut3lines[73] <- '  }'
mut3lines[74] <- '//return 0;'

mut3makelines <- readLines("methodFiles/mutual3/makefile")
mut3makelines[4] <- "MAIN=my_mutual3"
mut3makelines[4] <- "TAG=my_mutual"

writeLines(mut3lines, con = "methodFiles/mutual3/my_mutual3.cpp")
writeLines(mut3makelines, con = "methodFiles/mutual3/my_makefile")

setwd("methodFiles/mutual3")
system("make")
setwd("../../")
