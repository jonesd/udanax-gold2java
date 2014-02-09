# UdanaxGold2Java - Translator

The Translator is a small Java app to automatically translate from the
Udanax-Gold Smalltalk(ish) source code into Java. 

For more information on Abora see: http://abora.dgjones.info/

Translating from Smalltalk source code to Java code is normally a very
difficult proposition as Smalltalk source does not include declared
types. Thankfully as a requirement for XOCs own translator to C++, or
their variant, the typing information is present in the source code.


## Using the Translator

The translator is exercised by running the parent maven install.

> cd ..
> mvn clean install -Dtranslate

You should see some logging info summarising the walk through of a
number of source Smalltalk files, and a larger number of generated
Java files. The process should take less than a minute on a reasonable
machine. You should find approximately 500 classes generated.

The generated Java files are placed in `../abora-gold/src/generated-sources/translator` directory
under the info.dgjones.abora.gold java package. 


## Extending the Translator

Good Luck! This code was meant to be a throw away weekend project but
I had to end up extending it quite significantly. Still there isn't
too much code here, and there is some test coverage of the in-method
transformations.

The JUnit tests are present in the `info.dgjones.abora.ug2java.tests.TestWriteMethod`
class, and can be run courtesy of maven:

> mvn test


TranslateSmalltalk.main(...) is the entry point taking an output
directory, and a number of source Smalltalk files to process.

The Smalltalk files are stored in a version of the classic Smalltalk
Chunk format which effectively has a sequence of chunks of text with a
terminating exclamation mark (!). A chunk can define a new class,
define a method of a specific method category and arbitrary Smalltalk
expressions. More than one class can be defined in each source file.

There are two major passes over the source by TranslateSmalltalk.

The first pass reads in each of the Smalltalk source files in turn.
For each included class a ClassWriter is created and initialised with
its name, superclass, etc. Also each related chunk is read and either
added to the classWriter as a simple comment or an instance or static
method. The Java package of a class is based on its Smalltalk class
category, and is recorded for each class so that appropriate Java
import statements can be generated later.

The second pass walks through each ClassWriter requesting it to write
out a suitable .java file for itself. At this point each of its methods
is translated into Java and written out as part of the class
definition.

The Smalltalk source for a method is parsed using a SmalltalkScanner
which returns a series of ScannerTokens. These are simple token
interpretation of the Smalltalk with type and value. This series of
ScannerTokens is in turn converted into a sequence of sub-instances of
JavaToken. A series of transformX methods then walk the sequence of
Java tokens applying suitable transformations to move things closer to
the desired form. One transformation is to remove references to self
if necessary, another converts create(...) method calls to a
constructor call for the appropriate class.

Writing out a text version of the Java tokens is accomplished by
simply visiting each token in turn and appending to the current method
output. No effort is expended to indent statements correctly and also
some extra spaces will be inserted against standard Java formatting.
Additionally the original Smalltalk source is appended at the end of
the method together with the file and line number from where it came
to help with further manual corrections as may be needed later.


## Future Translator Improvements

I'm not sure how much further effort I am going to expend on the
translator, but considering the level of Java compilation problems
present in the translation code there is scope for lots of
improvements :-)

- Bugs trying to find beginning of expression
- Bracketting if/while expressions
- Convert more of the block cases; DiskManager.consistent, forEach,
etc
- Use Java abstract method/class support
- Cast if appropriate to result of make(...) calls
- Sort out IntegerVar/BooleanVar support, which may require working out
the type of fields and calls to make up for lack of C++ operator
overloading for converting between these classes and primitives and
equivalents of primitive operations such as +
- Indent resulting Java code rather than relying on an external source
formatter to be applied against the generated code
- Converting to a more standard parse tree may clear up some of the
code
- Try and define the transformations as parse-tree modifications in a
Refactoring or XLST fashion rather than existing primitive Java code
- Gain documents for XOCs own translator to C++ for a better handle
on what should be happening - for example class attributes such as ON.CLIENT
- Tidying up the code!


## Copyright and Licence

Abora Gold is Copyright 2003, 2014 David G Jones

Licensed under MIT X-11.

Substantial portions of the code are from the Udanax-Gold project and are Copyright 1979-1999 Udanax.com and licensed under MIT-X11. 
