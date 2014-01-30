/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.sysadm;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.sysadm.FeArchiver;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Used for transferring information to and from external storage medium. This protocol is
 * still expected to evolve.
 */
public class FeArchiver extends Heaper {

/*
udanax-top.st:19202:
Heaper subclass: #FeArchiver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-sysadm'!
*/
/*
udanax-top.st:19206:
FeArchiver comment:
'Used for transferring information to and from external storage medium. This protocol is still expected to evolve.'!
*/
/*
udanax-top.st:19208:
(FeArchiver getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
/*
udanax-top.st:19240:
FeArchiver class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19243:
(FeArchiver getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeArchiver.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Copy the entire contents of a set of Works onto secondary storage. Requires
 * read permission on all the Works (or the authority of the System Archive Club, which can
 * read anything). The medium is an Edition describing the kind of device on which to write
 * the backup. The result and the list of Works are wrapped as Sets, the medium as a
 * StorageMedium.
 * Returns the set of Works which were in fact successfully backed up.
 */
public FeEdition archive(FeEdition works, FeEdition medium) {
	Dean.shouldImplement();
	return null;
/*
udanax-top.st:19213:FeArchiver methodsFor: 'accessing'!
{FeEdition CLIENT} archive: works {FeEdition} with: medium {FeEdition}
	"Essential.  Copy the entire contents of a set of Works onto secondary storage. Requires read permission on all the Works (or the authority of the System Archive Club, which can read anything). The medium is an Edition describing the kind of device on which to write the backup. The result and the list of Works are wrapped as Sets, the medium as a StorageMedium.
	Returns the set of Works which were in fact successfully backed up."
	
	Dean shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Essential. Mark the contents of a set of Works as archived so that they can be discarded
 * from the online disk. Requires System Admin authority.
 */
public void markArchived(FeEdition edition) {
	Dean.shouldImplement();
/*
udanax-top.st:19220:FeArchiver methodsFor: 'accessing'!
{void CLIENT} markArchived: edition {FeEdition}
	"Essential. Mark the contents of a set of Works as archived so that they can be discarded from the online disk. Requires System Admin authority."
	
	Dean shouldImplement!
*/
}
/**
 * Essential.  Restore information from a backup tape. If a set of Works is specified, then
 * restores only them from the backup medium, otherwise just reads the entire contents. Must
 * have edit authority on Works which are restored. (Is this the right authority? What to do
 * about history?)
 * Returns the Works which were restored from tape.
 */
public FeEdition restore(FeEdition works, FeEdition medium) {
	Dean.shouldImplement();
	return null;
/*
udanax-top.st:19225:FeArchiver methodsFor: 'accessing'!
{FeEdition CLIENT} restore: works {FeEdition | NULL}
	with: medium {FeEdition}
	"Essential.  Restore information from a backup tape. If a set of Works is specified, then restores only them from the backup medium, otherwise just reads the entire contents. Must have edit authority on Works which are restored. (Is this the right authority? What to do about history?)
	Returns the Works which were restored from tape."
	
	Dean shouldImplement.
	^NULL "fodder"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:19235:FeArchiver methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19237:FeArchiver methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeArchiver make() {
	return new FeArchiver();
/*
udanax-top.st:19248:FeArchiver class methodsFor: 'create'!
{FeArchiver CLIENT} make
	^self create!
*/
}
/**
 * {FeEdition CLIENT} archive: works {FeEdition} with: medium {FeEdition}
 * {void CLIENT} markArchived: edition {FeEdition}
 * {FeEdition CLIENT} restore: works {FeEdition | NULL} with: medium {FeEdition}
 */
public static void infostProtocol() {
/*
udanax-top.st:19254:FeArchiver class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeEdition CLIENT} archive: works {FeEdition} with: medium {FeEdition}
{void CLIENT} markArchived: edition {FeEdition}
{FeEdition CLIENT} restore: works {FeEdition | NULL} with: medium {FeEdition}
"!
*/
}
public FeArchiver() {
/*

Generated during transformation
*/
}
public FeArchiver(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
