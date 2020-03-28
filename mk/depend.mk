eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  eg/minimal.hs
mk/All.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  mk/Toplibs.hs
src/Test/Framework/Providers/LeanCheck: \
  mk/toplibs
src/Test/Framework/Providers/LeanCheck.o: \
  src/Test/Framework/Providers/LeanCheck.hs
tests/test.o: \
  tests/test.hs \
  src/Test/Framework/Providers/LeanCheck.hs
tests/test: \
  tests/test.hs \
  mk/toplibs
