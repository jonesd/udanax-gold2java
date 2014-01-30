/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.gchooks;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This executor manages objects that need to release non-Heaper storage on finalization.
 */
public class DeleteExecutor extends XnExecutor {

	protected static PtrArray StorageArray;
	protected static WeakPtrArray StorageHolders;
/*
udanax-top.st:15847:
XnExecutor subclass: #DeleteExecutor
	instanceVariableNames: ''
	classVariableNames: '
		StorageArray {void vector star} 
		StorageHolders {WeakPtrArray} '
	poolDictionaries: ''
	category: 'Xanadu-gchooks'!
*/
/*
udanax-top.st:15853:
DeleteExecutor comment:
'This executor manages objects that need to release non-Heaper storage on finalization.'!
*/
/*
udanax-top.st:15855:
(DeleteExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:15879:
DeleteExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:15882:
(DeleteExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DeleteExecutor.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute(int estateIndex) {
	PtrArray storage;
	storage = (PtrArray) StorageArray.at(estateIndex);
	if (storage != null) {
		storage.delete();
	}
	StorageArray.put(estateIndex, null);
/*
udanax-top.st:15860:DeleteExecutor methodsFor: 'invoking'!
{void} execute: estateIndex {Int32}
	| storage {void star} |
	storage := StorageArray at: estateIndex.
	storage ~~ NULL ifTrue: [
		storage delete].
	StorageArray at: estateIndex put: NULL.!
*/
}
public DeleteExecutor() {
	super();
/*
udanax-top.st:15869:DeleteExecutor methodsFor: 'protected: create'!
create
	super create!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:15874:DeleteExecutor methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:15876:DeleteExecutor methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void registerHolder(Heaper holder, PtrArray storage) {
	int slot;
	if (StorageArray == null) {
		XnExecutor exec;
		/* Removed translateOnly */
		StorageArray = PtrArray.nulls(32);
		exec = new DeleteExecutor();
		StorageHolders = (WeakPtrArray) WeakPtrArray.make(exec, 32);
	}
	slot = StorageHolders.indexOf(null);
	if (slot == -1) {
		slot = StorageHolders.count();
		/* Removed translateOnly */
		StorageArray = (PtrArray) StorageArray.copyGrow(16);
		StorageHolders = (WeakPtrArray) (StorageHolders.copyGrow(16));
	}
	StorageArray.put(slot, storage);
	StorageHolders.store(slot, holder);
/*
udanax-top.st:15887:DeleteExecutor class methodsFor: 'accessing'!
{void} registerHolder: holder {Heaper} with: storage {void star}
	| slot {Int32} |
	StorageArray == NULL ifTrue: [
		| exec {XnExecutor} |
		'DeleteExecutor::StorageArray = new void* [32];
		memset (DeleteExecutor::StorageArray, 0, 32 * sizeof(void*));' translateOnly.
		[StorageArray := PtrArray nulls: 32] smalltalkOnly.
		exec := DeleteExecutor create.
		StorageHolders := WeakPtrArray make: exec with: 32].
	slot := StorageHolders indexOf: NULL.
	slot == -1 ifTrue: [
		slot := StorageHolders count.
		'void ** newArray = new void* [slot + 16];
		memset(&newArray[slot], 0, 16 * sizeof(void*));
		MEMMOVE(newArray, DeleteExecutor::StorageArray, (int)slot);
		delete DeleteExecutor::StorageArray;
		DeleteExecutor::StorageArray = newArray;' translateOnly.
		[StorageArray := StorageArray copyGrow: 16] smalltalkOnly.
		StorageHolders := (StorageHolders copyGrow: 16) cast: WeakPtrArray].
	StorageArray at: slot put: storage.
	StorageHolders at: slot store: holder.!
*/
}
public static void unregisterHolder(Heaper holder, PtrArray storage) {
	int slot;
	slot = StorageHolders.indexOfEQ(holder);
	while (slot != -1 && (slot < StorageHolders.count() && ((StorageArray.at(slot)) != storage))) {
		slot = StorageHolders.indexOfEQ(holder, slot + 1);
	}
	if (slot == -1 || ((StorageArray.at(slot)) != storage)) {
		throw new AboraRuntimeException(AboraRuntimeException.SANITY_VIOLATION);
	}
	StorageArray.put(slot, null);
	StorageHolders.store(slot, null);
/*
udanax-top.st:15909:DeleteExecutor class methodsFor: 'accessing'!
{void} unregisterHolder: holder {Heaper} with: storage {void star}
	| slot {Int32} |
	slot := StorageHolders indexOfEQ: holder.
	[slot ~= -1 and: [slot < StorageHolders count and: [(StorageArray at: slot) ~~ storage]]] whileTrue: [
		slot := StorageHolders indexOfEQ: holder with: slot + 1].
	(slot == -1 or: [(StorageArray at: slot) ~~ storage]) ifTrue: [
		Heaper BLAST: #SanityViolation].
	StorageArray at: slot put: NULL.
	StorageHolders at: slot store: NULL.!
*/
}
public static void linkTimeNonInherited() {
	StorageArray = null;
	StorageHolders = null;
/*
udanax-top.st:15921:DeleteExecutor class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	StorageArray := NULL.
	StorageHolders := NULL.!
*/
}
public DeleteExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static void registerHolder(Heaper holder, String storage) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
