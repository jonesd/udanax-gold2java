/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java;



public class HashHelper {

	public static int hashForEqual(Class clazz) {
		//TODO guess!
		return hashForEqual(clazz.getName());
	}
	
	public static int hashForEqual(int value) {
		//TODO guess!
		return Math.abs(value);
	}

	public static int hashForEqual(double d) {
		//TODO guess!
		return (int)Math.abs(d);
	}

	public static int hashForEqual(float f) {
		//TODO guess!
		return (int)Math.abs(f);
	}

	public static int hashForEqual(String s) {
		//TODO bad!! Probably shouldn't rely on Java hashCode behaviour
		return s.hashCode();
	}

}
