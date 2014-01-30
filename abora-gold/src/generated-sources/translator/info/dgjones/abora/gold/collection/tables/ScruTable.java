/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.OffsetScruTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.Signal;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Please read class comment for ScruSet first.
 * Like Sets, Tables represent collections of Heapers, and provide protocol for storing,
 * retrieving, and iterating over the collection.  However, Tables in addition provide an
 * organization for the Heapers collected together in the range of a Table:  A Table can also
 * be seen as a collection of associations between keys and values.  A particular Table
 * object has a particular domain coordinateSpace, and all keys in that Table are positions
 * in that coordinate space.  For each position in a Table''s coordinate space there is at
 * most one value which it maps to.  This value may be any arbitrary Heaper.  The same Heaper
 * may appear as value for several keys.
 * When iterating over the contents of a Table with a Stepper, the normal elements enumerated
 * by the Stepper are values (i.e., range elements) of the Table.  However,
 * ScruTable::stepper returns a TableStepper (a subclass of Stepper) which provides aditional
 * protocol of accessing the key corresponding to the current value.  (see ScruTable::stepper
 * and TableStepper.)
 */
public class ScruTable extends Heaper {

	protected static Signal NotInTableSignal;
	protected static Signal WrongCoordSpaceSignal;
/*
udanax-top.st:46847:
Heaper subclass: #ScruTable
	instanceVariableNames: ''
	classVariableNames: '
		NotInTableSignal {Signal smalltalk} 
		WrongCoordSpaceSignal {Signal smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:46853:
ScruTable comment:
'Please read class comment for ScruSet first.
	
	Like Sets, Tables represent collections of Heapers, and provide protocol for storing, retrieving, and iterating over the collection.  However, Tables in addition provide an organization for the Heapers collected together in the range of a Table:  A Table can also be seen as a collection of associations between keys and values.  A particular Table object has a particular domain coordinateSpace, and all keys in that Table are positions in that coordinate space.  For each position in a Table''s coordinate space there is at most one value which it maps to.  This value may be any arbitrary Heaper.  The same Heaper may appear as value for several keys. 
	
	When iterating over the contents of a Table with a Stepper, the normal elements enumerated by the Stepper are values (i.e., range elements) of the Table.  However, ScruTable::stepper returns a TableStepper (a subclass of Stepper) which provides aditional protocol of accessing the key corresponding to the current value.  (see ScruTable::stepper and TableStepper.)'!
*/
/*
udanax-top.st:46859:
(ScruTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:47235:
ScruTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:47238:
(ScruTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ScruTable.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The kind of elements used to index into the table are Positions of this
 * coordinate space. Therefore, the domain of this table is an XuRegion in this
 * coordinate space.
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46864:ScruTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"The kind of elements used to index into the table are Positions of this 
	coordinate space. Therefore, the domain of this table is an XuRegion in this 
	coordinate space."
	self subclassResponsibility!
*/
}
/**
 * Return the number of domain elements, which is to say, the number of associations.
 * 'table->count()' should be equivalent to 'table->domain()->count()'.
 * Used to say: 'Return the number of range elements'.  This seems clearly wrong.
 */
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46871:ScruTable methodsFor: 'accessing'!
{IntegerVar} count
	"Return the number of domain elements, which is to say, the number of associations.
	'table->count()' should be equivalent to 'table->domain()->count()'.
	
	Used to say: 'Return the number of range elements'.  This seems clearly wrong."
	
	self subclassResponsibility.!
*/
}
/**
 * Return an XuRegion representing a snapshot of the current domain.
 * 'table->domain()->hasMember(p)' iff 'table->fetch(p) !!= NULL'.
 */
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46879:ScruTable methodsFor: 'accessing'!
{XnRegion} domain
	"Return an XuRegion representing a snapshot of the current domain.  
	'table->domain()->hasMember(p)' iff 'table->fetch(p) !!= NULL'."
	self subclassResponsibility.!
*/
}
/**
 * Return the range element at the domain position key. The routine will return
 * NULL if the position is not in the table.
 */
public Heaper fetch(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46885:ScruTable methodsFor: 'accessing'!
{Heaper} fetch: key {Position} 
	"Return the range element at the domain position key. The routine will return 
	NULL if the position is not in the table."
	self subclassResponsibility!
*/
}
/**
 * Return the range element at the domain position key. BLAST if the position is
 * not in the table.
 */
public Heaper get(Position key) {
	Heaper tmp;
	tmp = fetch(key);
	if (tmp == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return tmp;
/*
udanax-top.st:46891:ScruTable methodsFor: 'accessing'!
{Heaper} get: key {Position} 
	"Return the range element at the domain position key. BLAST if the position is 
	not in the table."
	| tmp {Heaper wimpy} |
	tmp _ self fetch: key.
	tmp == NULL ifTrue: [Heaper BLAST: #NotInTable.
		^NULL].
	^tmp!
*/
}
/**
 * A snapshot of the current range elements of the table collected together into
 * an ImmuSet.
 */
public ImmuSet range() {
	SetAccumulator acc;
	acc = SetAccumulator.make();
	Stepper stomper = stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper obj = (Heaper) stomper.fetch();
		if (obj == null) {
			continue ;
		}
		acc.step(obj);
	}
	stomper.destroy();
	return (ImmuSet) acc.value();
/*
udanax-top.st:46901:ScruTable methodsFor: 'accessing'!
{ImmuSet of: Heaper} range
	"A snapshot of the current range elements of the table collected together into 
	an ImmuSet."
	| acc {SetAccumulator} |
	acc _ SetAccumulator make.
	self stepper forEach: [:obj {Heaper} | acc step: obj].
	^acc value cast: ImmuSet!
*/
}
/**
 * Return a table which contains only the intersection of this table's domain and
 * the domain specified by 'region'.
 * table->subTable(r)->domain()->isEqual( table->domain()->intersect(r) ).
 * It is unspecified whether the resulting table starts as a snapshot of a subset of
 * me, after which we go our own ways; or whether the resulting table is a view
 * onto a subset of me, such that changes to me are also visible to him. Of
 * course, subclasses may specify more. If you want to ensure snapshot behavior,
 * do 'table->subTable(r)->asImmuTable()'.
 * NOTE: In the future we may specify snapshot behavior or we may specify view
 * behavior. As a client this shouldn't effect you. However, if you implement a
 * new kind of ScruTable, please let us know. Also, if you have an opinion as to
 * which way you'd like the specification tightened up, please tell us.
 */
public ScruTable subTable(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46910:ScruTable methodsFor: 'accessing'!
{ScruTable} subTable: region {XnRegion} 
	"Return a table which contains only the intersection of this table's domain and 
	the domain specified by 'region'. 
	table->subTable(r)->domain()->isEqual( table->domain()->intersect(r) ). 
	
	It is unspecified whether the resulting table starts as a snapshot of a subset of 
	me, after which we go our own ways; or whether the resulting table is a view 
	onto a subset of me, such that changes to me are also visible to him. Of 
	course, subclasses may specify more. If you want to ensure snapshot behavior, 
	do 'table->subTable(r)->asImmuTable()'. 
	
	NOTE: In the future we may specify snapshot behavior or we may specify view 
	behavior. As a client this shouldn't effect you. However, if you implement a 
	new kind of ScruTable, please let us know. Also, if you have an opinion as to 
	which way you'd like the specification tightened up, please tell us."
	self subclassResponsibility!
*/
}
/**
 * Return a ScruTable with the domain of the receiver transformed by the Dsp.
 * 'table->transformedBy(d)->fetch(p)' is equivalent to
 * 'table->fetch(d->of(p))'.
 * See ScruTable::subTable for caveats regarding whether we return a snapshot
 * or a view. All the same caveats apply.
 */
public ScruTable transformedBy(Dsp dsp) {
	return new OffsetScruTable(this, dsp);
/*
udanax-top.st:46928:ScruTable methodsFor: 'accessing'!
{ScruTable} transformedBy: dsp {Dsp} 
	"Return a ScruTable with the domain of the receiver transformed by the Dsp. 
	'table->transformedBy(d)->fetch(p)' is equivalent to 
	'table->fetch(d->of(p))'. 
	
	See ScruTable::subTable for caveats regarding whether we return a snapshot 
	or a view. All the same caveats apply."
	^ OffsetScruTable create: self with: dsp!
*/
}
/**
 * See ScruTable::isEqual
 */
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:46940:ScruTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"See ScruTable::isEqual"
	^Heaper takeOop!
*/
}
/**
 * Returns whether the two ScruTables have exactly the same mapping from
 * keys to values at the moment. 'a->contentsEqual(b)' is equivalent to
 * 'a->asImmuTable()->isEqual(b->asImmuTable())'. See ScruTable::contentsEqual
 */
public boolean contentsEqual(ScruTable other) {
	TableStepper myStepper;
	Heaper otherValue;
	if (other.count() != count()) {
		return false;
	}
	if ( ! (other.coordinateSpace().isEqual(coordinateSpace()))) {
		return false;
	}
	myStepper = stepper();
	while (myStepper.hasValue()) {
		otherValue = other.fetch(myStepper.position());
		if (otherValue == null) {
			return false;
		}
		if ( ! (otherValue.isEqual(myStepper.fetch()))) {
			return false;
		}
		myStepper.step();
	}
	return true;
/*
udanax-top.st:46945:ScruTable methodsFor: 'testing'!
{BooleanVar} contentsEqual: other {ScruTable}
	"Returns whether the two ScruTables have exactly the same mapping from 
	keys to values at the moment. 'a->contentsEqual(b)' is equivalent to 
	'a->asImmuTable()->isEqual(b->asImmuTable())'. See ScruTable::contentsEqual"
	| myStepper {TableStepper} otherValue {Heaper} |
	(other count ~= self count) ifTrue: [^false].
	(other coordinateSpace isEqual: self coordinateSpace) ifFalse: [^false].
	myStepper _ self stepper.
	[myStepper hasValue] whileTrue:
		[otherValue _ other fetch: myStepper position.
		otherValue == NULL ifTrue: [^false].
		(otherValue isEqual: myStepper fetch) ifFalse: [^false].
		myStepper step].
	^true!
*/
}
/**
 * Has the same relationship to contentsEqual that hashForEqual has to isEqual.
 * I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'.
 * The same complex caveats apply as to the stability and portability of the
 * hash values as apply for hashForEqual.  See ScruSet contentsHash.
 */
public int contentsHash() {
	XnRegion dom;
	dom = domain();
	if (dom.isEmpty()) {
		return coordinateSpace().hashForEqual() * 17;
	}
	else {
		if (dom instanceof IntegerRegion) {
			IntegerRegion ints = (IntegerRegion) dom;
			return ints.start() + ints.stop() + count() + coordinateSpace().hashForEqual();
		}
		else {
			return count() + coordinateSpace().hashForEqual();
		}
	}
/*
udanax-top.st:46961:ScruTable methodsFor: 'testing'!
{UInt32} contentsHash
	"Has the same relationship to contentsEqual that hashForEqual has to isEqual. 
	I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'. 
	The same complex caveats apply as to the stability and portability of the 
	hash values as apply for hashForEqual.  See ScruSet contentsHash."
	| dom {XnRegion} |
	dom _ self domain.
	dom isEmpty
		ifTrue: [^self coordinateSpace hashForEqual*17]
		ifFalse: [dom
			cast: IntegerRegion into: [ :ints |
				^ints start DOTasLong
				+ ints stop DOTasLong
				+ self count DOTasLong
				+ self coordinateSpace hashForEqual]
			others:
				[^self count DOTasLong 
				+ self coordinateSpace hashForEqual]].
	^UInt32Zero "fodder"!
*/
}
/**
 * includesKey is used to test for the presence of a key->value pair in the
 * table.  This routine returns true if there is a value present at the specified
 * key, and false otherwise.
 * 'table->includesKey(p)' iff 'table->domain()->hasMember(p)'.
 */
public boolean includesKey(Position key) {
	return (fetch(key)) != null;
/*
udanax-top.st:46982:ScruTable methodsFor: 'testing'!
{BooleanVar} includesKey: key {Position}
	"includesKey is used to test for the presence of a key->value pair in the
	table.  This routine returns true if there is a value present at the specified
	key, and false otherwise.
	'table->includesKey(p)' iff 'table->domain()->hasMember(p)'."
	^ (self fetch: key) ~~ NULL!
*/
}
/**
 * Is there anything in the table?
 * 'table->isEmpty()' iff 'table->domain()->isEmpty()'.
 */
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46990:ScruTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	"Is there anything in the table? 
	'table->isEmpty()' iff 'table->domain()->isEmpty()'."
	self subclassResponsibility!
*/
}
/**
 * All MuTable subclasses have equality based on identity
 * (now and forever equal. Many ScruTable subclasses will
 * represent an aspect of another table. Therefore they have
 * hashForEqual and isEqual: based on both their contained
 * table, and the aspect that they represent. Thus, two similar
 * views onto the same MuTable are now (and forever) equal.
 * The hashForEqual: must use exactly the same aspects for
 * the hash as get used for isEqual:. ImmuTables all use
 * contentBased comparison operations.
 */
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:46996:ScruTable methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	"All MuTable subclasses have equality based on identity 
	(now and forever equal. Many ScruTable subclasses will 
	represent an aspect of another table. Therefore they have 
	hashForEqual and isEqual: based on both their contained 
	table, and the aspect that they represent. Thus, two similar 
	views onto the same MuTable are now (and forever) equal. 
	The hashForEqual: must use exactly the same aspects for 
	the hash as get used for isEqual:. ImmuTables all use 
	contentBased comparison operations."
	
	self subclassResponsibility!
*/
}
/**
 * Return a TableStepper which will enumerate my key->value mappings. The
 * Stepper component of the TableStepper protocol will just enumerate my values
 * (as that is what I'm a container *of*--the keys are simply how I organize my
 * contents). TableStepper provides additional protocol to ascetain the current
 * key. See TableStepper and XuRegion::stepper. The TableStepper I produce given
 * an order must enumerate keys according to the same rules which specify how
 * XuRegion::stepper must enumerate positions. I am not asserting that the actual
 * orders are the same, only that the correctness criteria on the allowable orders
 * are the same.
 * Keeping in mind that we are talking about equivalence of specification
 * and not equivalence of particular behavior, the following two statements
 * are equivalent:
 * {
 * SPTR(TableStepper) stomp = table->stepper(o);
 * SPTR(Position) key;
 * FOR_EACH(Heaper,val,stomp, {
 * key = stomp->key();
 * doSomethingWith(key, val);
 * });
 * }
 * and
 * {
 * SPTR(Heaper) val;
 * SPTR(ImmuTable) snapShot = table->asImmuTable();
 * FOR_EACH(Position,key,(snapShot->domain()->stepper(o)), {
 * val = snapShot->get (key);
 * doSomethingWith(key, val);
 * });
 * }
 */
public TableStepper stepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47011:ScruTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	"Return a TableStepper which will enumerate my key->value mappings. The 
	Stepper component of the TableStepper protocol will just enumerate my values 
	(as that is what I'm a container *of*--the keys are simply how I organize my 
	contents). TableStepper provides additional protocol to ascetain the current 
	key. See TableStepper and XuRegion::stepper. The TableStepper I produce given 
	an order must enumerate keys according to the same rules which specify how 
	XuRegion::stepper must enumerate positions. I am not asserting that the actual 
	orders are the same, only that the correctness criteria on the allowable orders 
	are the same. 
	
	Keeping in mind that we are talking about equivalence of specification 
	and not equivalence of particular behavior, the following two statements 
	are equivalent: 
	{
		SPTR(TableStepper) stomp = table->stepper(o);
		SPTR(Position) key;
		FOR_EACH(Heaper,val,stomp, {
			key = stomp->key();
			doSomethingWith(key, val);
		});
	}
	
	and
	
	{
		SPTR(Heaper) val;
		SPTR(ImmuTable) snapShot = table->asImmuTable();
		FOR_EACH(Position,key,(snapShot->domain()->stepper(o)), {
			val = snapShot->get (key);
			doSomethingWith(key, val);
		});
	}
			
	"
	self subclassResponsibility!
*/
}
/**
 * Iff I contain exactly one range element, return it.  Otherwise BLAST.
 * The idea for this message is taken from the THE function of ONTIC
 * (reference McAllester)
 */
public Heaper theOne() {
	TableStepper stepper;
	Heaper result;
	if (count() != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
	}
	stepper = stepper();
	result = stepper.fetch();
	stepper.destroy();
	return result;
/*
udanax-top.st:47050:ScruTable methodsFor: 'enumerating'!
{Heaper} theOne
	"Iff I contain exactly one range element, return it.  Otherwise BLAST.
	The idea for this message is taken from the THE function of ONTIC
	(reference McAllester)"
	| stepper {TableStepper} result {Heaper} |
	self count ~~ 1 ifTrue:
		[ Heaper BLAST: #NotOneElement ].
	stepper _ self stepper.
	result _ stepper fetch.
	stepper destroy.
	^ result!
*/
}
/**
 * Return a side-effect-free snapshot of my current contents.
 * See ScruSet::asImmuSet.
 */
public ImmuTable asImmuTable() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47065:ScruTable methodsFor: 'conversion'!
{ImmuTable} asImmuTable
	"Return a side-effect-free snapshot of my current contents.
	See ScruSet::asImmuSet."
	self subclassResponsibility!
*/
}
/**
 * Return a side-effectable version of the same table.
 * See ScruSet::asMuSet.
 */
public MuTable asMuTable() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47071:ScruTable methodsFor: 'conversion'!
{MuTable} asMuTable
	"Return a side-effectable version of the same table.
	See ScruSet::asMuSet."
	self subclassResponsibility!
*/
}
/**
 * Return the subTable with the domain of all positions whose values are equal to
 * value.  Defined by analogy with corresponding Waldo-level operation.
 */
public ScruTable backfollowFrom(Heaper value) {
	throw new UnimplementedException();
/*
udanax-top.st:47079:ScruTable methodsFor: 'smalltalk: backfollow'!
{ScruTable} backfollowFrom: value {Heaper} 
	"Return the subTable with the domain of all positions whose values are equal to 
	value.  Defined by analogy with corresponding Waldo-level operation."
	self unimplemented.
	"value == NULL
		ifTrue: [^self]
		ifFalse: [^BackfollowTable create: self with: value]"!
*/
}
public void printOn(PrintWriter stream) {
	stream.print(getAboraClass().name());
	printOnWithSimpleSyntax(stream, "(", ", ", ")");
/*
udanax-top.st:47089:ScruTable methodsFor: 'printing'!
{void} printOn: stream {ostream reference}
	stream << self getCategory name.
	self printOnWithSimpleSyntax: stream with: '(' with: ', ' with: ')'!
*/
}
/**
 * Print the contents of the table as
 * <open> key1
 */
public void printOnWithSimpleSyntax(PrintWriter oo, String open, String sep, String close) {
	/* -> */
	/*  value1 <sep> key2  */
	/* -> */
	/*  value2 <sep> ... <sep> keyN  */
	/* -> */
	/*  valueN <close>.
	For example, 'table->printOnWithSyntax(oo,  */
	/* { */
	/* ,  */
	/* ,  */
	/* ,  */
	/* } */
	/* );' may result in
	'{3->Foo(), 5->Bar()}'.
	
	One wierd but convenient special case: if the domain space is an IntegerSpace, 
	we print the keys according to the way IntegerVars print, not the way 
	XuIntegers print.
	
	For yet more fine-grained control over printing, see the ScruTable::printOnWithSyntax 
	with 5 arguments. */
	printOnWithSyntax(oo, open, "->", sep, close);
/*
udanax-top.st:47093:ScruTable methodsFor: 'printing'!
{void} printOnWithSimpleSyntax: oo {ostream reference} 
	with: open {char star} 
	with: sep {char star} 
	with: close {char star}
	"Print the contents of the table as
	<open> key1 ""->"" value1 <sep> key2 ""->"" value2 <sep> ... <sep> keyN ""->"" valueN <close>.
	For example, 'table->printOnWithSyntax(oo, ""{"", "", "", ""}"");' may result in
	'{3->Foo(), 5->Bar()}'.
	
	One wierd but convenient special case: if the domain space is an IntegerSpace, 
	we print the keys according to the way IntegerVars print, not the way 
	XuIntegers print.
	
	For yet more fine-grained control over printing, see the ScruTable::printOnWithSyntax 
	with 5 arguments."
	
	self printOnWithSyntax: oo
		with: open
		with: '->'
		with: sep
		with: close.!
*/
}
/**
 * Print the contents of the table as
 * <open> key1 <map> value1 <sep> key2 <map> value2
 * <sep> ... <sep> keyN <map> valueN <close>.
 * For example, 'table->printOnWithSyntax(oo,
 */
public void printOnWithSyntax(PrintWriter stream, String open, String map, String sep, String close) {
	/* { */
	/* , 
	 */
	/* => */
	/* ,  */
	/* ,  */
	/* ,  */
	/* } */
	/* );' may result in 
	'{3=>Foo(), 5=>Bar()}'. 
	
	One wierd but convenient special case: if the 
	domain space is an IntegerSpace, 
	we print the keys according to the way IntegerVars 
	print, not the way 
	XuIntegers print. */
	TableStepper stomp;
	stream.print(open);
	if ( ! (isEmpty())) {
		stomp = stepper();
		while (stomp.hasValue()) {
			Heaper cast1 = stomp.position();
			if (cast1 instanceof IntegerPos) {
				IntegerPos xui = (IntegerPos) cast1;
				stream.print(xui.asIntegerVar());
			}
			else {
				stream.print(stomp.position());
			}
			stream.print(map);
			stream.print(stomp.fetch());
			stomp.step();
			if (stomp.hasValue()) {
				stream.print(sep);
			}
		}
		stomp.destroy();
	}
	stream.print(close);
/*
udanax-top.st:47115:ScruTable methodsFor: 'printing'!
{void} printOnWithSyntax: stream {ostream reference} 
	with: open {char star} 
	with: map {char star} 
	with: sep {char star} 
	with: close {char star} 
	"Print the contents of the table as 
	<open> key1 <map> value1 <sep> key2 <map> value2 
	<sep> ... <sep> keyN <map> valueN <close>. 
	For example, 'table->printOnWithSyntax(oo, ""{"", 
	""=>"", "", "", ""}"");' may result in 
	'{3=>Foo(), 5=>Bar()}'. 
	
	One wierd but convenient special case: if the 
	domain space is an IntegerSpace, 
	we print the keys according to the way IntegerVars 
	print, not the way 
	XuIntegers print."
	| stomp {TableStepper} |
	stream << open.
	self isEmpty ifFalse:
		[stomp _ self stepper.
		[stomp hasValue]
			whileTrue: 
				[stomp position 
					cast: IntegerPos into: [:xui |
						stream << xui asIntegerVar]
					others: [stream << stomp position].
				stream << map << stomp fetch.
				stomp step.
				stomp hasValue ifTrue: [stream << sep]].
		stomp destroy].
	stream << close!
*/
}
/**
 * Return the length of the run starting at position key. A run is defined as a
 * contiguous (charming) sequence of domain positions mapping to equal (isEqual)
 * objects. Charming is defined as: Given a charming region R, for all a,c which
 * are elements of R and a >= b >= c, b is an element of R. Where '>=' is
 * according to the 'isGE' message.
 * NOTE: We may retire the above definition of charming. The possible changes
 * will only effect spaces which aren't fully ordered. OrderedRegions,
 * TreeRegions, and IntegerRegions will be unaffected, as any future definition of
 * 'runAt' will be equivalent for them.
 */
public XnRegion runAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47151:ScruTable methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	"Return the length of the run starting at position key. A run is defined as a 
	contiguous (charming) sequence of domain positions mapping to equal (isEqual) 
	objects. Charming is defined as: Given a charming region R, for all a,c which 
	are elements of R and a >= b >= c, b is an element of R. Where '>=' is 
	according to the 'isGE' message. 
	
	NOTE: We may retire the above definition of charming. The possible changes 
	will only effect spaces which aren't fully ordered. OrderedRegions, 
	TreeRegions, and IntegerRegions will be unaffected, as any future definition of 
	'runAt' will be equivalent for them."
	self subclassResponsibility!
*/
}
/**
 * A new one whose initial state is my current state, but that doesn't track
 * changes. Note that there is no implication that these can be 'destroy'ed
 * separately, because (for example) an ImmuTable just returns itself
 */
public ScruTable copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47167:ScruTable methodsFor: 'creation'!
{ScruTable} copy
	"A new one whose initial state is my current state, but that doesn't track 
	changes. Note that there is no implication that these can be 'destroy'ed 
	separately, because (for example) an ImmuTable just returns itself"
	self subclassResponsibility!
*/
}
/**
 * Return an empty table just like the current one. The 'size' argument is a hint
 * about how big the count of the table will probably become (so that the new
 * table can be prepared to grow to that size efficiently).
 */
public ScruTable emptySize(int size) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:47174:ScruTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar} 
	"Return an empty table just like the current one. The 'size' argument is a hint 
	about how big the count of the table will probably become (so that the new 
	table can be prepared to grow to that size efficiently)."
	self subclassResponsibility!
*/
}
public ScruTable() {
	super();
/*
udanax-top.st:47183:ScruTable methodsFor: 'protected: creation'!
create
	super create!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public boolean includesIntKey(int aKey) {
	return includesKey(IntegerPos.make(aKey));
/*
udanax-top.st:47188:ScruTable methodsFor: 'overloads'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	^self includesKey: aKey integer!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public Heaper intFetch(int key) {
	return fetch(IntegerPos.make(key));
/*
udanax-top.st:47193:ScruTable methodsFor: 'overloads'!
{Heaper} intFetch: key {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	^ self fetch: key integer!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public Heaper intGet(int key) {
	Heaper tmp;
	tmp = intFetch(key);
	if (tmp == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return tmp;
/*
udanax-top.st:47198:ScruTable methodsFor: 'overloads'!
{Heaper} intGet: key {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	| tmp {Heaper wimpy} |
	tmp _ self intFetch: key.
	tmp == NULL ifTrue: [Heaper BLAST: #NotInTable.
		^NULL].
	^tmp!
*/
}
/**
 * Unboxed version.  See class comment for XuInteger
 */
public XnRegion runAtInt(int key) {
	return runAt(IntegerPos.make(key));
/*
udanax-top.st:47206:ScruTable methodsFor: 'overloads'!
{XnRegion} runAtInt: key {IntegerVar}
	"Unboxed version.  See class comment for XuInteger"
	^ self runAt: key integer!
*/
}
/**
 * implement default argument of NULL
 */
public TableStepper stepper() {
	return stepper(null);
/*
udanax-top.st:47213:ScruTable methodsFor: 'smalltalk: enumerating'!
stepper
	"implement default argument of NULL"
	^self stepper: NULL!
*/
}
/*
udanax-top.st:47219:ScruTable methodsFor: 'smalltalk: special'!
asOrderedCollection
	"convert for use with Smalltalk MVC stuff"
	| stomp {TableStepper} res {OrderedCollection} |
	res _ OrderedCollection new: self count.
	stomp _ self stepper.
	[stomp hasValue]
		whileTrue: [res add: stomp get.
			stomp step].
	^res.!
*/
/*
udanax-top.st:47230:ScruTable methodsFor: 'smalltalk: special'!
{void} do: aBlock {BlockClosure of: Heaper}
	self stepper forEach: aBlock!
*/
/*
udanax-top.st:47243:ScruTable class methodsFor: 'exceptions: exceptions'!
problems.NotInTable
	^self signals: #(NotInTable)!
*/
public ScruTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
