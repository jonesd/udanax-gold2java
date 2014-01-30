/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.Initializer;
import info.dgjones.abora.gold.java.missing.XnReadFile;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Filename;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.MainDummy;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;
import java.io.PrintWriter;

/**
 * A dummy class on which to hang the main that reads in an rc file.
 */
public class MainDummy extends Heaper {

/*
udanax-top.st:28218:
Heaper subclass: #MainDummy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:28222:
MainDummy comment:
'A dummy class on which to hang the main that reads in an rc file.'!
*/
/*
udanax-top.st:28224:
(MainDummy getOrMakeCxxClassDescription)
	friends:
'/- friends for class MainDummy -/
friend int  main (int argc, char* * argv);';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:28237:
MainDummy class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28240:
(MainDummy getOrMakeCxxClassDescription)
	friends:
'/- friends for class MainDummy -/
friend int  main (int argc, char* * argv);';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MainDummy.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:28233:MainDummy methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static void run(String filename) {
	XUUMAIN(2, (Array.with(filename)));
/*
udanax-top.st:28249:MainDummy class methodsFor: 'smalltalk: booting'!
{void} run: filename
	self XU.U.MAIN: 2 with: (Array with: filename)!
*/
}
public static int runString(String string) {
	Initializer.enterDoMain();
	try {
		Rcvr rc;
		Heaper next;
		rc = TextyXcvrMaker.make().makeRcvr((TransferSpecialist.make((Cookbook.makeString("boot")))), ( new XnReadFile(AboraSupport.readStream(string))));
		next = rc.receiveHeaper();
		while (next != null) {
			if (next instanceof Thunk) {
				Thunk thunk = (Thunk) next;
				thunk.execute();
			}
			next = rc.receiveHeaper();
		}
		rc.destroy();
	}
	finally {
		Initializer.exitDoMain();
	}
	return 0;
/*
udanax-top.st:28252:MainDummy class methodsFor: 'smalltalk: booting'!
{void} runString: string 
	Initializer doMain:
	[| rc {Rcvr} next {Heaper | NULL} |
	rc _ TextyXcvrMaker make 
				makeRcvr: (TransferSpecialist make: (Cookbook make.String: 'boot'))
				with: (XnReadFile create: string readStream).
	next _ rc receiveHeaper.
	[next ~~ NULL] whileTrue: 
		[next cast: Thunk into: [:thunk | thunk execute] others: [].
		next _ rc receiveHeaper].
	rc destroy].
	^Int32Zero!
*/
}
public static void toFileRunString(Filename fileName, String string) {
	PrintWriter aStream;
	PrintWriter saveCerr;
	aStream = fileName.writeStream();
	saveCerr = AboraSupport.logger;
	try {
		Rcvr rc;
		Heaper next;
		AboraSupport.logger = aStream;
		Someone.knownBug();
		/* only accepts UInt8Arrays */
		rc = TextyXcvrMaker.make().makeRcvr((TransferSpecialist.make((Cookbook.makeString("boot")))), ( new XnReadFile(AboraSupport.readStream(string))));
		next = rc.receiveHeaper();
		while (next != null) {
			if (next instanceof Thunk) {
				Thunk thunk = (Thunk) next;
				thunk.execute();
			}
			next = rc.receiveHeaper();
		}
		rc.destroy();
	}
	finally {
		AboraSupport.logger = saveCerr;
		aStream.close();
	}
/*
udanax-top.st:28265:MainDummy class methodsFor: 'smalltalk: booting'!
{void} toFile: fileName {Filename} runString: string {String}
	| aStream saveCerr |
	aStream _ fileName writeStream.
	saveCerr _ cerr.
	[| rc {Rcvr} next {Heaper | NULL} |
	cerr _ aStream.
	self knownBug. "only accepts UInt8Arrays"
	rc _ TextyXcvrMaker make 
				makeRcvr: (TransferSpecialist make: (Cookbook make.String: 'boot'))
				with: (XnReadFile create: string readStream).
	next _ rc receiveHeaper.
	[next ~~ NULL] whileTrue: 
		[next cast: Thunk into: [:thunk | thunk execute] others: [].
		next _ rc receiveHeaper].
	rc destroy] valueNowOrOnUnwindDo: 
		[cerr _ saveCerr.
		aStream close]!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(Rcvr.class, "CurrentMainReceiver", Emulsion.globalEmulsion(), null);
	AboraSupport.defineFluid(Heaper.class, "MainActiveThunk", Emulsion.globalEmulsion(), null);
/*
udanax-top.st:28286:MainDummy class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	Rcvr defineFluid: #CurrentMainReceiver with: Emulsion globalEmulsion with: [NULL].
	Heaper defineFluid: #MainActiveThunk with: Emulsion globalEmulsion with: [NULL].!
*/
}
public static int XUUMAIN(int argc, Array argv) {
	int stackObject;
	AboraSupport.translateOnly();
	{
		/* StackExaminer::stackEnd(&stackObject); */
	}
	Initializer.enterDoMain(argc, argv);
	try {
		Rcvr rc;
		Heaper next;
		if (argc < 2) {
			AboraSupport.logger.print("usage: ");
			AboraSupport.logger.print((argv.at(0)));
			AboraSupport.logger.print(" rcFileName\n"+
"");
			return 1;
		}
		rc = TextyXcvrMaker.make().makeRcvr((TransferSpecialist.make((Cookbook.makeString("boot")))), (XnReadFile.make((argv.at(1)))));
		Object currentMainReceiverOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentMainReceiver, rc);
		try {
			next = ((Rcvr) CurrentMainReceiver.fluidGet()).receiveHeaper();
			while (next != null) {
				Object mainActiveThunkOldValue = AboraBlockSupport.enterFluidBindDuring(MainActiveThunk, next);
				try {
					if (next instanceof Thunk) {
						Thunk thunk = (Thunk) next;
						thunk.execute();
					}
				}
				finally {
					AboraBlockSupport.exitFluidBindDuring(MainActiveThunk, mainActiveThunkOldValue);
				}
				next = ((Rcvr) CurrentMainReceiver.fluidGet()).receiveHeaper();
			}
			((Rcvr) CurrentMainReceiver.fluidGet()).destroy();
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentMainReceiver, currentMainReceiverOldValue);
		}
		return 0;
	}
	finally {
		Initializer.exitDoMain();
	}
/*
udanax-top.st:28292:MainDummy class methodsFor: 'global: booting'!
{int} XU.U.MAIN: argc {int} with: argv {char star vector}
	| stackObject {Int32} |
	[StackExaminer] USES.
	'StackExaminer::stackEnd(&stackObject);' translateOnly.
	Initializer with: argc with: argv doMain:
	[| rc {Rcvr} next {Heaper | NULL} |
	argc < 2 ifTrue: [cerr << 'usage: ' << (argv at: Int32Zero) << ' rcFileName
'.	^1].
	rc _ TextyXcvrMaker make 
			makeRcvr: (TransferSpecialist make: (Cookbook make.String: 'boot')) 
			with: (XnReadFile make: (argv at: 1)).
	CurrentMainReceiver fluidBind: rc during:
		[next _ CurrentMainReceiver fluidGet receiveHeaper.
		[next ~~ NULL] whileTrue:
			[MainActiveThunk fluidBind: next during:
				[next cast: Thunk into: [:thunk | thunk execute] others: []].
			next _ CurrentMainReceiver fluidGet  receiveHeaper].
			CurrentMainReceiver fluidGet destroy].
	^Int32Zero]!
*/
}
/**
 * @deprecated
 */
public static int main(int argc, String argv) {
	throw new PasseException();
/*
udanax-top.st:28314:MainDummy class methodsFor: 'smalltalk: passe'!
{int} main: argc {int} with: argv {char star vector}
	
	self passe!
*/
}
public MainDummy() {
/*

Generated during transformation
*/
}
public MainDummy(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
