/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import java.lang.reflect.Method;

import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.java.Fn;
import org.abora.gold.lock.Encrypter;
import org.abora.gold.xpp.basic.Category;

public class EncrypterConstructor extends Fn {

	public EncrypterConstructor(Category category, String selector) {
		super(category, selector);
	}

	public Encrypter invokeFunction(UInt8Array publicKey, UInt8Array privateKey) {
		Object[] arguments = new Object[] {publicKey, privateKey};
		return (Encrypter)invokeStaticWith(arguments);
	}

	public static EncrypterConstructor make(Category category) {
		return new EncrypterConstructor(category, "make");
	}
	
	protected boolean isMatchingMethod(Method m, String selector) {
		return super.isMatchingMethod(m, selector) &&
			m.getParameterTypes().length == 2 && m.getParameterTypes()[0].equals(UInt8Array.class) && m.getParameterTypes()[1].equals(UInt8Array.class);
	}


}
