/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.gchooks.CloseExecutor;
import info.dgjones.abora.gold.java.AboraSocketSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.FDListener;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is the superclass for Listeners that use Berkeley UNIX sockets.
 */
public class FDListener extends ServerChunk {

	protected int myFD;
/*
udanax-top.st:50826:
ServerChunk subclass: #FDListener
	instanceVariableNames: 'myFD {int NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:50830:
FDListener comment:
'This is the superclass for Listeners that use Berkeley UNIX sockets.'!
*/
/*
udanax-top.st:50832:
(FDListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:50878:
FDListener class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:50881:
(FDListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FDListener.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public int descriptor() {
	return myFD;
/*
udanax-top.st:50837:FDListener methodsFor: 'accessing'!
{int} descriptor
	^myFD.!
*/
}
/**
 * Attempt to execute another chunk.  Return whether there is more to be done.
 */
public boolean execute() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:50841:FDListener methodsFor: 'accessing'!
{BooleanVar} execute
	"Attempt to execute another chunk.  Return whether there is more to be done."
	self subclassResponsibility.!
*/
}
/**
 * There should be data waiting on this FD. Return TRUE if I am still in a reasonable state
 * to continue, FALSE if not (in which case the Listener will be destroyed by the caller)
 */
public boolean shouldBeReady() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:50846:FDListener methodsFor: 'accessing'!
{BooleanVar} shouldBeReady
	"There should be data waiting on this FD. Return TRUE if I am still in a reasonable state to continue, FALSE if not (in which case the Listener will be destroyed by the caller)"
	self subclassResponsibility.!
*/
}
public FDListener() {
	super();
	AboraSupport.smalltalkOnly();
	{
		myFD = 0;
	}
	AboraSupport.translateOnly();
	{
		/* myFD = (int) Int32Zero; */
	}
/*
udanax-top.st:50853:FDListener methodsFor: 'creation'!
create
	super create.
	[myFD _ Int32Zero] smalltalkOnly.
	'myFD = (int) Int32Zero;' translateOnly.!
*/
}
public void destruct() {
	AboraSupport.smalltalkOnly();
	{
		AboraSocketSupport.close(myFD);
	}
	AboraSupport.translateOnly();
	{
		/* close (myFD); */
	}
	super.destruct();
/*
udanax-top.st:50859:FDListener methodsFor: 'creation'!
{void} destruct
	[myFD close] smalltalkOnly.
	'close (myFD);' translateOnly.
	super destruct.!
*/
}
public void registerFor(int anFD) {
	myFD = anFD;
	CloseExecutor.registerHolder(this, anFD);
	ServerLoop.introduceChunk(this);
/*
udanax-top.st:50865:FDListener methodsFor: 'creation'!
{void} registerFor: anFD {int}
	myFD _ anFD.
	CloseExecutor registerHolder: self with: anFD.
	ServerLoop introduceChunk: self!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:50873:FDListener methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:50875:FDListener methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void initTimeNonInherited() {
	/* Removed translateOnly */
/*
udanax-top.st:50886:FDListener class methodsFor: 'smalltalk: init'!
initTimeNonInherited
'
#ifdef unix
	signal(SIGPIPE, SIG_IGN);
#endif
' translateOnly!
*/
}
/*
udanax-top.st:50896:FDListener class methodsFor: 'exceptions: exceptions'!
problems.SOCKET.U.ERRS
	^self signals: #(SOCKET.U.RECV.U.ERROR SOCKET.U.SEND.U.ERROR)!
*/
public FDListener(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
