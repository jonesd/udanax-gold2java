/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java.missing;

import java.lang.reflect.Method;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.Fn;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.xpp.basic.Category;

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
