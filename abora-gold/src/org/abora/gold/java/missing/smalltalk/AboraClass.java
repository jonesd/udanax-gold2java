/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.io.PrintWriter;

import org.abora.gold.xpp.basic.Heaper;

public class AboraClass extends Heaper {

	private final Class c;
	//TODO just making up the preorder stuff!
	private final int preorderNumber;
	
	private static int nextPreorderNumber = 0;
	
	public AboraClass(Class c) {
		super();
		this.c = c;
		this.preorderNumber = nextPreorderNumber++;
	}

	public static int getPreorderMax() {
		if (nextPreorderNumber == 0) {
			throw new IllegalStateException("Preorder not initialized yet!");
		}
		return nextPreorderNumber;
	}
	
	public String fetchAttribute(Object attr) {
		throw new UnsupportedOperationException();
	}

	public boolean hasAttribute(Object attr) {
		throw new UnsupportedOperationException();
	}

	public boolean isConcretePackage() {
		throw new UnsupportedOperationException();
	}

	public boolean inheritsFrom(AboraClass clazz) {
		return clazz.c.isAssignableFrom(c);
	}

	public String name() {
		String fullClassName = c.getName();
		return shortName(fullClassName);
	}

	private String shortName(String fullClassName) {
		return fullClassName.substring(fullClassName.lastIndexOf(".") + 1);
	}

	public int preorderNumber() {
		//TODO made up!
		return preorderNumber;
	}

	public Object perform(String mySelector) {
		throw new UnsupportedOperationException();
	}

	public Object perform(String mySelector, Object arg1) {
		throw new UnsupportedOperationException();
	}

	public Object perform(String mySelector, Object arg1, Object arg2) {
		throw new UnsupportedOperationException();
	}

	public Object perform(String mySelector, Object arg1, Object arg2, Object arg3) {
		throw new UnsupportedOperationException();
	}

	public Object perform(String mySelector, Object arg1, Object arg2, Object arg3, Object arg4) {
		throw new UnsupportedOperationException();
	}

	public OrderedCollection allInstances() {
		throw new UnsupportedOperationException();
	}

	public boolean isMeta() {
		throw new UnsupportedOperationException();
	}

	public void initImageEmulsion() {
		throw new UnsupportedOperationException();
	}

	public boolean isEqualOrSubclassOf(AboraClass class1) {
		throw new UnsupportedOperationException();
	}
	
	public Class getJavaClass() {
		return c;
	}
	
	public void printOn(PrintWriter oo) {
		oo.print(shortName(getClass().getName()));
		oo.print("(");
		oo.print(name());
		oo.print(")");
	}
}
