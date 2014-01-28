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
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

public class ExcludeStaticHeaper implements MethodTransformation {

	private static final List KEEP;
	static {
		List list = new ArrayList();
		list.add("passe");
		//list.add("takeOop");
		
		list.add("Heaper");
		list.add("actualHashForEqual");
		list.add("destroy");
		list.add("destruct");
		//list.add("destructor");
		//list.add("delete");
		list.add("equals");
		//list.add("getCategory");
		list.add("hash");
		list.add("hashForEqual");
		list.add("isEqual");
		list.add("printOn");
		list.add("sendSelfTo");

		KEEP = Collections.unmodifiableList(list);
	}

	
	public void transform(JavaMethod javaMethod) {
		if (javaMethod.javaClass.className.equals("Heaper")) {
			if (!KEEP.contains(javaMethod.name)) {
				javaMethod.shouldInclude = false;
			}
		}
	}

}