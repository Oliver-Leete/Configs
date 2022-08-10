using ReTest;

ReTest.load(ARGS[1]);
try
    retest(join(ARGS[2:end], " "), spin=false, verbose=Inf)
catch
end
retest(join(ARGS[2:end], " "), dry=true, marks=true, verbose=Inf)
