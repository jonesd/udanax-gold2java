/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.schunk.ChunkCleaner;
import info.dgjones.abora.gold.srvloop.ListenerEmulsion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

/**
 * This is the superclass for all the Chunks.  Chunks represent pieces of the server that run
 * for a while, then return control.  Subclasses include Listeners that wait for input.
 * When manually destroyed, this class flags itself for cleanup after any current
 * request is finished--myEnding state is alive, alive in request, destruction requested, and
 * ready for destruction.
 */
public class ServerChunk extends Heaper {

	protected Array myFluidSpace;
	protected int myEndingState;
	protected static Emulsion SecretEmulsion;
/*
udanax-top.st:50657:
Heaper subclass: #ServerChunk
	instanceVariableNames: '
		myFluidSpace {char star}
		myEndingState {Int32}'
	classVariableNames: 'SecretEmulsion {Emulsion} '
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:50663:
ServerChunk comment:
'This is the superclass for all the Chunks.  Chunks represent pieces of the server that run for a while, then return control.  Subclasses include Listeners that wait for input.    When manually destroyed, this class flags itself for cleanup after any current
request is finished--myEnding state is alive, alive in request, destruction requested, and ready for destruction.'!
*/
/*
udanax-top.st:50666:
(ServerChunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:50745:
ServerChunk class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:50748:
(ServerChunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ServerChunk.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public boolean destroyOKIfRequested() {
	if (myEndingState == ServerChunk.inRequestFlag() || (myEndingState == ServerChunk.destroyRequestedFlag())) {
		myEndingState = ServerChunk.destroyReadyFlag();
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:50671:ServerChunk methodsFor: 'protected: accessing'!
{BooleanVar} destroyOKIfRequested
	(myEndingState == ServerChunk inRequestFlag or: [myEndingState == ServerChunk destroyRequestedFlag]) ifTrue: [
		myEndingState := ServerChunk destroyReadyFlag.
		^ true]
	ifFalse: [^false]!
*/
}
public boolean destroyPending() {
	return myEndingState == ServerChunk.destroyRequestedFlag();
/*
udanax-top.st:50677:ServerChunk methodsFor: 'protected: accessing'!
{BooleanVar} destroyPending
	^ myEndingState == ServerChunk destroyRequestedFlag.!
*/
}
public void inRequest() {
	myEndingState = ServerChunk.inRequestFlag();
/*
udanax-top.st:50680:ServerChunk methodsFor: 'protected: accessing'!
{void} inRequest
	myEndingState := ServerChunk inRequestFlag.!
*/
}
public void notInRequest() {
	if (myEndingState == ServerChunk.destroyRequestedFlag()) {
		myEndingState = ServerChunk.destroyReadyFlag();
	}
	else {
		if (myEndingState == ServerChunk.inRequestFlag()) {
			myEndingState = ServerChunk.aliveFlag();
		}
	}
/*
udanax-top.st:50683:ServerChunk methodsFor: 'protected: accessing'!
{void} notInRequest
	myEndingState == ServerChunk destroyRequestedFlag
		ifTrue: [myEndingState := ServerChunk destroyReadyFlag]
		ifFalse: [myEndingState == ServerChunk inRequestFlag ifTrue: [myEndingState := ServerChunk aliveFlag]]!
*/
}
/**
 * Returns TRUE if this chunk wants to be deleted after deregistration.
 */
public boolean shouldDestroy() {
	return myEndingState == ServerChunk.destroyReadyFlag();
/*
udanax-top.st:50690:ServerChunk methodsFor: 'testing'!
{BooleanVar} shouldDestroy
	"Returns TRUE if this chunk wants to be deleted after deregistration."
	^ myEndingState == ServerChunk destroyReadyFlag!
*/
}
/**
 * Attempt to execute another chunk.  Return whether there is more to be done.
 */
public boolean execute() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:50696:ServerChunk methodsFor: 'accessing'!
{BooleanVar} execute
	"Attempt to execute another chunk.  Return whether there is more to be done."
	self subclassResponsibility.!
*/
}
public Array fluidSpace() {
	return myFluidSpace;
/*
udanax-top.st:50701:ServerChunk methodsFor: 'accessing'!
{char star} fluidSpace
	^myFluidSpace.!
*/
}
public Array fluidSpace(Array aFluidSpace) {
	return myFluidSpace = aFluidSpace;
/*
udanax-top.st:50705:ServerChunk methodsFor: 'accessing'!
{char star} fluidSpace: aFluidSpace {char star}
	^myFluidSpace _ aFluidSpace.!
*/
}
/**
 * ServerChunks are destroyed explicitly in the server loop.
 */
public void destruct() {
	ServerChunk saveChunk;
	if (myFluidSpace != null) {
		saveChunk = CurrentChunk;
		CurrentChunk = this;
		ServerChunk.emulsion().destructAll();
		CurrentChunk = saveChunk;
	}
	ServerLoop.removeChunk(this);
	ChunkCleaner.beClean();
	super.destruct();
/*
udanax-top.st:50711:ServerChunk methodsFor: 'protected: destruct'!
{void} destruct
	"ServerChunks are destroyed explicitly in the server loop."
	| saveChunk {ServerChunk} |
	(myFluidSpace ~~ NULL) ifTrue: [
		saveChunk _ CurrentChunk.
		CurrentChunk _ self.
		ServerChunk emulsion destructAll.
		CurrentChunk _ saveChunk.].
	ServerLoop removeChunk: self.
	ChunkCleaner beClean.
	super destruct.!
*/
}
public ServerChunk() {
	super();
	myFluidSpace = null;
	myEndingState = 0;
/*
udanax-top.st:50725:ServerChunk methodsFor: 'creation'!
create
	super create.
	myFluidSpace _ NULL.
	myEndingState := Int32Zero.!
*/
}
public void destroy() {
	if (myEndingState == ServerChunk.aliveFlag() || (myEndingState == ServerChunk.destroyReadyFlag())) {
		super.destroy();
	}
	else {
		if (myEndingState == ServerChunk.destroyRequestedFlag()) {
			throw new AboraRuntimeException(AboraRuntimeException.ALREADY_DESTROYED);
		}
		myEndingState = ServerChunk.destroyRequestedFlag();
	}
/*
udanax-top.st:50731:ServerChunk methodsFor: 'creation'!
{void} destroy
	(myEndingState == ServerChunk aliveFlag or: [myEndingState == ServerChunk destroyReadyFlag])
		ifTrue: [super destroy]
		ifFalse: [
			myEndingState == ServerChunk destroyRequestedFlag ifTrue: [Heaper BLAST: #AlreadyDestroyed].
			myEndingState := ServerChunk destroyRequestedFlag]!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:50740:ServerChunk methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:50742:ServerChunk methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static Emulsion emulsion() {
	/* Removed smalltalkOnly */
	if (SecretEmulsion == null) {
		SecretEmulsion = new ListenerEmulsion();
	}
	return SecretEmulsion;
/*
udanax-top.st:50753:ServerChunk class methodsFor: 'accessing'!
{Emulsion} emulsion
	[SecretEmulsion == nil ifTrue: [SecretEmulsion _ NULL]] smalltalkOnly.
	(SecretEmulsion == NULL) ifTrue: [
		SecretEmulsion _ ListenerEmulsion new create].
	^SecretEmulsion.!
*/
}
public static void cleanupGarbage() {
	SecretEmulsion = null;
/*
udanax-top.st:50761:ServerChunk class methodsFor: 'smalltalk: init'!
cleanupGarbage
	SecretEmulsion _ NULL!
*/
}
public static void linkTimeNonInherited() {
	ServerChunk.defineGlobal(CURRENT_CHUNK, null);
	SecretEmulsion = null;
/*
udanax-top.st:50764:ServerChunk class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	ServerChunk defineGlobal: #CurrentChunk with: NULL.
	SecretEmulsion _ NULL.!
*/
}
public static int aliveFlag() {
	return 0;
/*
udanax-top.st:50770:ServerChunk class methodsFor: 'protected: accessing'!
{Int32 INLINE} aliveFlag
	^ Int32Zero!
*/
}
public static int destroyReadyFlag() {
	return 3;
/*
udanax-top.st:50773:ServerChunk class methodsFor: 'protected: accessing'!
{Int32 INLINE} destroyReadyFlag
	^ 3!
*/
}
public static int destroyRequestedFlag() {
	return 2;
/*
udanax-top.st:50776:ServerChunk class methodsFor: 'protected: accessing'!
{Int32 INLINE} destroyRequestedFlag
	^ 2!
*/
}
public static int inRequestFlag() {
	return 1;
/*
udanax-top.st:50779:ServerChunk class methodsFor: 'protected: accessing'!
{Int32 INLINE} inRequestFlag
	^ 1!
*/
}
public ServerChunk(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static void defineGlobal(String globalName, Heaper initialValue) {
	AboraSupport.defineGlobalRecipe(globalName, initialValue);
/*

Generated during transformation: AddMethod
*/
}
}
