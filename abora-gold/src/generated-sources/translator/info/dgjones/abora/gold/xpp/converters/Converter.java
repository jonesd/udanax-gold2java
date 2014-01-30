/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.converters;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.converters.CategoryTable;
import info.dgjones.abora.gold.xpp.converters.Converter;

/**
 * Converters exist to convert an object that is of one data type to some other data type.
 * In order to understand why we use Converters for this, we first have to understand why the
 * two obvious C++ means for doing so are inadequate.
 * First, we could send a conversion message to the object that we''re converting from.  This
 * has the disadvantage that the module which provides the object must know all the types
 * that we might want to convert it to.  This effectively prevents the easy addition of new
 * types to an existing system (without coordinating overhead).
 * Second, we could use pseudo-constructors for the new type which were overloaded based of
 * the type of the argument (to be converted from). The problem is that overloading only
 * happens based on compile time type, and we want to invoke the converter that is most
 * specific based on the run-time type.  Let''s say that "p" is declared as pointer to
 * Position, but at the moment it points to an Integer. If a converter has been provided by
 * someone for converting Integers to the target type we would prefer that one.  The CHOOSE
 * macro doesn''t help, because that only converts run time types to particular compile time
 * types which the user of the CHOOSE macro thought to list.
 * Instead, one declares a static instance of each Converter which (at initialization time)
 * posts itself to a table of all converters, indexed by the from-class and the to-class.  At
 * run-time, when we use the CONVERT macro to convert an object to a desired type, the most
 * specific converter based on the from-type hierarchy is selected.  I believe that currently
 * any compatable to-type is acceptable (with no preference).  The selected converter''s from
 * type may be a superclass of the from-object, and the converter''s to-type may be any
 * subclass of the requested to type.
 * Example:
 * "CONVERT(XuRegion,p)" when p is bound to a position will return a singleton region of the
 * same coordinate space as that of the position.  In Smalltalk, one says "p convert:
 * XuRegion".
 * See Scaffold and WaldoMaker for services which are similar to that provided by Converter.
 * Eventually we hope to unify some of these.
 */
public class Converter extends Heaper {

	protected static CategoryTable AllConverters;
	protected static Converter LastConverter;
	protected static Category LastFrom;
	protected static Category LastTo;
/*
Xanadu-Xpp-Converters.st:0:
Heaper subclass: #Converter
	instanceVariableNames: ''
	classVariableNames: '
		AllConverters {CategoryTable} 
		LastConverter {Converter star} 
		LastFrom {Category star} 
		LastTo {Category star} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Converters'!
*/
/*
Xanadu-Xpp-Converters.st:8:
Converter comment:
'Converters exist to convert an object that is of one data type to some other data type.  In order to understand why we use Converters for this, we first have to understand why the two obvious C++ means for doing so are inadequate.
	
	First, we could send a conversion message to the object that we''re converting from.  This has the disadvantage that the module which provides the object must know all the types that we might want to convert it to.  This effectively prevents the easy addition of new types to an existing system (without coordinating overhead).
	
	Second, we could use pseudo-constructors for the new type which were overloaded based of the type of the argument (to be converted from). The problem is that overloading only happens based on compile time type, and we want to invoke the converter that is most specific based on the run-time type.  Let''s say that "p" is declared as pointer to Position, but at the moment it points to an Integer. If a converter has been provided by someone for converting Integers to the target type we would prefer that one.  The CHOOSE macro doesn''t help, because that only converts run time types to particular compile time types which the user of the CHOOSE macro thought to list.
	
	Instead, one declares a static instance of each Converter which (at initialization time) posts itself to a table of all converters, indexed by the from-class and the to-class.  At run-time, when we use the CONVERT macro to convert an object to a desired type, the most specific converter based on the from-type hierarchy is selected.  I believe that currently any compatable to-type is acceptable (with no preference).  The selected converter''s from type may be a superclass of the from-object, and the converter''s to-type may be any subclass of the requested to type.
	
	Example:
	
	"CONVERT(XuRegion,p)" when p is bound to a position will return a singleton region of the same coordinate space as that of the position.  In Smalltalk, one says "p convert: XuRegion".
	
	See Scaffold and WaldoMaker for services which are similar to that provided by Converter.  Eventually we hope to unify some of these.'!
*/
/*
Xanadu-Xpp-Converters.st:22:
(Converter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #OBSOLETE; add: #EQ; add: #DEFERRED; yourself)!
*/
/*
Xanadu-Xpp-Converters.st:53:
Converter class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-Converters.st:56:
(Converter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #OBSOLETE; add: #EQ; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Converter.class).setAttributes( new Set().add("OBSOLETE").add("EQ").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * remember the kind of converter this is.  It is in the parameters passed to this by
 * subclass
 * constructors that a concrete Converter class declares what types it knows how to convert
 * to what.
 * See any subclass's constructor.
 */
public Converter(Category from, Category to) {
	super();
	CategoryTable subtable;
	subtable = (CategoryTable) (AllConverters.fetchSuperOf(from));
	if (subtable == null) {
		subtable = CategoryTable.make();
		AllConverters.introduce(from, 
		/* was store: */
		subtable);
	}
	subtable.introduce(to, this);
/*
Xanadu-Xpp-Converters.st:27:Converter methodsFor: 'protected: creation'!
create: from {Category} with: to {Category}
	"remember the kind of converter this is.  It is in the parameters passed to this by subclass
	 constructors that a concrete Converter class declares what types it knows how to convert to what.
	 See any subclass's constructor."
	| subtable {CategoryTable} |
	super create.
	subtable _ (AllConverters fetchSuperOf: from) cast: CategoryTable.
	subtable == NULL ifTrue:
		[subtable _ CategoryTable make.
		AllConverters at: from introduce:"was store:" subtable].
	subtable at: to introduce: self.!
*/
}
/**
 * perform the conversion.  This is defined by those who define a new converter, but not
 * directly called
 * by clients.  Clients invoke conversion purely by using the CONVERT macro
 */
public Heaper convert(Heaper value) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Converters.st:41:Converter methodsFor: 'accessing'!
{Heaper} convert: value {Heaper}
	"perform the conversion.  This is defined by those who define a new converter, but not directly called
	 by clients.  Clients invoke conversion purely by using the CONVERT macro"
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
Xanadu-Xpp-Converters.st:48:Converter methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
Xanadu-Xpp-Converters.st:50:Converter methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * find the best available converter.  See class comment for definition of best.
 */
public static Converter fetchConverter(Category from, Category to) {
	CategoryTable candidates;
	Converter result;
	if (LastFrom == from && (LastTo == to)) {
		return LastConverter;
	}
	candidates = (CategoryTable) (AllConverters.fetchSuperOf(from));
	if (candidates == null) {
		return null;
	}
	result = (Converter) (candidates.fetchSubOf(to));
	LastConverter = result;
	LastFrom = from;
	LastTo = to;
	return result;
/*
Xanadu-Xpp-Converters.st:61:Converter class methodsFor: 'accessing'!
{Converter} fetchConverter: from {Category} with: to {Category}
	"find the best available converter.  See class comment for definition of best."
	| candidates {CategoryTable} result {Converter} |
	(LastFrom == from and: [LastTo == to]) ifTrue: [ ^ LastConverter ].
	candidates _ (AllConverters fetchSuperOf: from) quickCast: CategoryTable.
	candidates == NULL ifTrue: [^NULL].
	result _ (candidates fetchSubOf: to) quickCast: Converter.
	LastConverter _ result.
	LastFrom _ from.
	LastTo _ to.
	^ result!
*/
}
/**
 * get the best converter and use it to actually do the conversion.
 * Clients should only use the CONVERT macro instead
 */
public static Heaper getConversion(Category to, Heaper value) {
	AboraSupport.translateOnly();
	{
		/* return getConverter (value->getCategory(), to)->convert(value); */
	}
	AboraSupport.smalltalkOnly();
	{
		return (Converter.getConverter(value.getCategory(), to)).convert(value);
	}
/*
Xanadu-Xpp-Converters.st:73:Converter class methodsFor: 'accessing'!
{Heaper} getConversion: to {Category} with: value {Heaper}
	"get the best converter and use it to actually do the conversion.  
	Clients should only use the CONVERT macro instead"
	
	'return getConverter (value->getCategory(), to)->convert(value);' translateOnly.
	[^(Converter getConverter: value getCategory with: to) convert: value] smalltalkOnly!
*/
}
/**
 * find the best available converter.  See class comment for definition of best.
 */
public static Converter getConverter(Category from, Category to) {
	Converter result;
	result = Converter.fetchConverter(from, to);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.CANT_CONVERT);
	}
	return result;
/*
Xanadu-Xpp-Converters.st:80:Converter class methodsFor: 'accessing'!
{Converter} getConverter: from {Category} with: to {Category}
	"find the best available converter.  See class comment for definition of best."
	| result {Converter} |
	result _ Converter fetchConverter: from with: to.
	result == NULL ifTrue: [Heaper BLAST: #CantConvert].
	^result!
*/
}
/**
 * create an instance of the converter.  The constructor places it into the AllConverters
 * table
 */
public static void initTimeInherited() {
	/* TODO newAllocType */
	new Converter();
	/* create a new instance of each subclass */
/*
Xanadu-Xpp-Converters.st:89:Converter class methodsFor: 'smalltalk: initialization'!
initTimeInherited
	"create an instance of the converter.  The constructor places it into the AllConverters table"
	self REQUIRES: Converter.
	(self new.AllocType: #PERSISTENT) create. "create a new instance of each subclass"!
*/
}
/**
 * create an instance and assign it to the class instance variable
 */
public static void initTimeNonInherited() {
	AllConverters = CategoryTable.make();
/*
Xanadu-Xpp-Converters.st:95:Converter class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	"create an instance and assign it to the class instance variable"
	self REQUIRES: CategoryTable.
	AllConverters _ CategoryTable make.!
*/
}
/**
 * create an instance and assign it to the class instance variable
 */
public static void linkTimeNonInherited() {
	AllConverters = null;
	LastFrom = null;
	LastTo = null;
	LastConverter = null;
/*
Xanadu-Xpp-Converters.st:100:Converter class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	"create an instance and assign it to the class instance variable"
	AllConverters _ NULL.
	LastFrom _ NULL.
	LastTo _ NULL.
	LastConverter _ NULL.!
*/
}
public static void suppressInitTimeInherited() {
/*
Xanadu-Xpp-Converters.st:107:Converter class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
Xanadu-Xpp-Converters.st:109:Converter class methodsFor: 'smalltalk: initialization'!
suppressLinkTimeInherited!
*/
}
/**
 * convert using the best available converter
 */
public static Heaper CONVERT(Category to, Heaper thing) {
	return ((getConverter(thing.getCategory(), to)).convert(thing));
/*
Xanadu-Xpp-Converters.st:113:Converter class methodsFor: 'smalltalk: macro'!
{Heaper} CONVERT: to {Category} with: thing {Heaper}
	"convert using the best available converter"
	^((self getConverter: thing getCategory with: to) convert: thing) cast: to!
*/
}
public Converter() {
/*

Generated during transformation
*/
}
public Converter(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
