cd abc
mxmlc -debug -output Test.swf -keep-as3-metadata=FooMeta -static-link-runtime-shared-libraries -swf-version=10 Test.as &&
./extractAbc Test.swf &&
rm -f Test.swf &&
./abcdump/abcdump Test.abc &> Test.abc.dump
