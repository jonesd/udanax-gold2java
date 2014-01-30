/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.sets;

import info.dgjones.abora.gold.collection.sets.ActualHashSet;
import info.dgjones.abora.gold.collection.sets.HashSet;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The HashSet class is an implementation of a MuTable set that can contain arbitrary
 * Heapers.  It uses the hashForEqual member function to get a hash value for the items
 * contained in the set.  The set establishes the equality of objects in the set through the
 * isEqual: function.
 * The implemention of HashSet is straightforward.  There are primitive tables used to store
 * pointers to the stored items (myHashEntries), and their corresponding hash values
 * (myHashValues).  The HashSet also maintain a current tally of the number of items in the
 * set.
 * definition: preferred location - the location calculated from item hashForEqual mod
 * tableSize.
 * The search routine first calculates the preferred location.  This is used as the first
 * location in the myHashEntries table to test for the presence of the item, and the search
 * proceeds forward (in a linear probe) from there.  If there is no object at that position,
 * the routine assumes the item is not in the table.  If there is an item there, the first
 * test is to check if the item''s hash matches the hash myHashValues at that position.  If
 * the match is successful, a full isEqual: test is performed.  If the isEqual: test
 * succeeds, the item is present in the set.  If either test fails, the entry at the desired
 * location is tested to see if it is closer to its own preferred location than the item (a
 * test which necessarily fails the first time).  If it is closer, the item is not in the
 * set.  (This extra test is what distinguishes an ordered hash set from an ordinary hash
 * set.  It often detects the absense of an item well before an empty slot is encountered,
 * and the advantage becomes pronounced as the set fills up.  Ordered hash sets with linear
 * probe beat ordinary hash sets with secondary clustering on misses (the big time eater),
 * yet they preserve linear probe''s easy deletion.)
 * On insertion to the set, the hash and probe sequence is essentially the same as the
 * search.  The main exception is that on a hash collision, the item bumps down any item that
 * is no farther than its own preferred position.
 * An example is perhaps in order:
 * the set contains items a, b, and c in table locations 3, 4, and 5.  Assume that a has
 * location 2 as its preferred location, while b and c both have location 4 as their
 * preferred location.  Now, if we attempt to add an item d to the table, and item d were to
 * have a preferred location of 3.  Since 3 is already occupied by something that is already
 * far from its preferred location, we probe for a another location.  At location 4, item d
 * is displaced by one from its preferred location.  Since b is in it''s preferred location
 * (4) d replaces it, and we move item b down.  Item c is in location 5 because it had
 * already been bumped from location 4 when b was inserted previously.  B again ties with c,
 * so it pushes it out of location 5, replacing it there.  Item c will end up in location 6.
 * This probe function minimizes the individual displacement of hash misses, while keeping
 * the most items in their preferred locations.
 * Note that, though the choice of which item to bump is obvious when the distances from home
 * are different, when they are equal we could have given preference to either the new or the
 * old item.  We chose to put the new item closer to its preferred location, on the
 * assumption that things entered recently are more likely to be looked up than things
 * entered long ago.
 * This algorithm was derived from a short discussion with Michael McClary (probably
 * completely missing his intended design - all mistakes are mine -- heh).
 * (Unfortunately, I wasn''t clear in the discussion.  Since hugh was unavailable when I
 * discovered this, I''ve taken the opportunity to practice with Smalltalk and corrected both
 * the explanation and the code rather than sending him a clarification. -- michael)
 */
public class HashSet extends MuSet {

/*
udanax-top.st:46261:
MuSet subclass: #HashSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Sets'!
*/
/*
udanax-top.st:46265:
HashSet comment:
'	The HashSet class is an implementation of a MuTable set that can contain arbitrary Heapers.  It uses the hashForEqual member function to get a hash value for the items contained in the set.  The set establishes the equality of objects in the set through the isEqual: function.
	
	The implemention of HashSet is straightforward.  There are primitive tables used to store pointers to the stored items (myHashEntries), and their corresponding hash values (myHashValues).  The HashSet also maintain a current tally of the number of items in the set.
	
	definition: preferred location - the location calculated from item hashForEqual mod tableSize.
	
	The search routine first calculates the preferred location.  This is used as the first location in the myHashEntries table to test for the presence of the item, and the search proceeds forward (in a linear probe) from there.  If there is no object at that position, the routine assumes the item is not in the table.  If there is an item there, the first test is to check if the item''s hash matches the hash myHashValues at that position.  If the match is successful, a full isEqual: test is performed.  If the isEqual: test succeeds, the item is present in the set.  If either test fails, the entry at the desired location is tested to see if it is closer to its own preferred location than the item (a test which necessarily fails the first time).  If it is closer, the item is not in the set.  (This extra test is what distinguishes an ordered hash set from an ordinary hash set.  It often detects the absense of an item well before an empty slot is encountered, and the advantage becomes pronounced as the set fills up.  Ordered hash sets with linear probe beat ordinary hash sets with secondary clustering on misses (the big time eater), yet they preserve linear probe''s easy deletion.)
	
	On insertion to the set, the hash and probe sequence is essentially the same as the search.  The main exception is that on a hash collision, the item bumps down any item that is no farther than its own preferred position.
	
	An example is perhaps in order:
	
	the set contains items a, b, and c in table locations 3, 4, and 5.  Assume that a has location 2 as its preferred location, while b and c both have location 4 as their preferred location.  Now, if we attempt to add an item d to the table, and item d were to have a preferred location of 3.  Since 3 is already occupied by something that is already far from its preferred location, we probe for a another location.  At location 4, item d is displaced by one from its preferred location.  Since b is in it''s preferred location (4) d replaces it, and we move item b down.  Item c is in location 5 because it had already been bumped from location 4 when b was inserted previously.  B again ties with c, so it pushes it out of location 5, replacing it there.  Item c will end up in location 6.  This probe function minimizes the individual displacement of hash misses, while keeping the most items in their preferred locations.
	
	Note that, though the choice of which item to bump is obvious when the distances from home are different, when they are equal we could have given preference to either the new or the old item.  We chose to put the new item closer to its preferred location, on the assumption that things entered recently are more likely to be looked up than things entered long ago.
	
This algorithm was derived from a short discussion with Michael McClary (probably completely missing his intended design - all mistakes are mine -- heh).
(Unfortunately, I wasn''t clear in the discussion.  Since hugh was unavailable when I discovered this, I''ve taken the opportunity to practice with Smalltalk and corrected both the explanation and the code rather than sending him a clarification. -- michael)'!
*/
/*
udanax-top.st:46285:
(HashSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class HashSet -/
friend class HashSetTester;';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:46355:
HashSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:46358:
(HashSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class HashSet -/
friend class HashSetTester;';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashSet.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public boolean hasMember(Heaper someone) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46293:HashSet methodsFor: 'accessing'!
{BooleanVar} hasMember: someone {Heaper}
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46296:HashSet methodsFor: 'accessing'!
{BooleanVar} isEmpty
	self subclassResponsibility!
*/
}
public ScruSet copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46301:HashSet methodsFor: 'creation'!
{ScruSet} copy
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46306:HashSet methodsFor: 'enumerating'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public Stepper stepper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46309:HashSet methodsFor: 'enumerating'!
{Stepper} stepper
	self subclassResponsibility!
*/
}
public Heaper theOne() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46312:HashSet methodsFor: 'enumerating'!
{Heaper} theOne
	self subclassResponsibility!
*/
}
public void introduce(Heaper anElement) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46317:HashSet methodsFor: 'adding-removing'!
{void} introduce: anElement {Heaper}
	self subclassResponsibility!
*/
}
public void remove(Heaper anElement) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46320:HashSet methodsFor: 'adding-removing'!
{void} remove: anElement {Heaper}
	self subclassResponsibility!
*/
}
/**
 * Add anElement to my set of members.  No semantic effect if anElement is already a member.
 */
public void store(Heaper anElement) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46323:HashSet methodsFor: 'adding-removing'!
{void} store: anElement {Heaper}
	"Add anElement to my set of members.  No semantic effect if anElement is already a member."
	
	self subclassResponsibility!
*/
}
/**
 * make anElement no longer be one of my members.  No semantic effect if it already isn't a
 * member.
 */
public void wipe(Heaper anElement) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46328:HashSet methodsFor: 'adding-removing'!
{void} wipe: anElement {Heaper}
	"make anElement no longer be one of my members.  No semantic effect if it already isn't a member."
	
	self subclassResponsibility!
*/
}
public ImmuSet asImmuSet() {
	if (isEmpty()) {
		return ImmuSet.make();
	}
	if (count() == 1) {
		return ImmuSet.make().with((theOne()));
	}
	return ImmuSet.make(this);
/*
udanax-top.st:46335:HashSet methodsFor: 'conversion'!
{ImmuSet} asImmuSet
	self isEmpty ifTrue: [^ ImmuSet make].
	self count == 1 ifTrue: [^ ImmuSet make with: (self theOne)].
	^ ImmuSet make: self!
*/
}
public MuSet asMuSet() {
	return (MuSet) copy();
/*
udanax-top.st:46340:HashSet methodsFor: 'conversion'!
{MuSet} asMuSet
	^ self copy quickCast: MuSet!
*/
}
public void printInternals(PrintWriter oo) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46345:HashSet methodsFor: 'private: testing access'!
{void} printInternals: oo {ostream reference}
	self subclassResponsibility!
*/
}
public Stepper immuStepper() {
	throw new UnimplementedException();
/*
udanax-top.st:46350:HashSet methodsFor: 'private: enumerating'!
{Stepper} immuStepper
	self unimplemented.
	^NULL!
*/
}
public static MuSet make() {
	return ActualHashSet.make();
/*
udanax-top.st:46366:HashSet class methodsFor: 'pseudo constructors'!
make
	^ActualHashSet make!
*/
}
public static MuSet makeHeaper(Heaper something) {
	ActualHashSet set;
	set = (ActualHashSet) ActualHashSet.makeIntegerVar(1);
	set.store(something);
	return set;
/*
udanax-top.st:46369:HashSet class methodsFor: 'pseudo constructors'!
make.Heaper: something {Heaper}
	| set {ActualHashSet} |
	set _ ActualHashSet make.IntegerVar: 1.
	set store: something.
	^ set!
*/
}
public static MuSet makeIntegerVar(int someSize) {
	return ActualHashSet.makeIntegerVar(someSize);
/*
udanax-top.st:46375:HashSet class methodsFor: 'pseudo constructors'!
make.IntegerVar: someSize {IntegerVar}
	^ ActualHashSet make.IntegerVar: someSize.!
*/
}
public HashSet() {
/*

Generated during transformation
*/
}
public HashSet(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
