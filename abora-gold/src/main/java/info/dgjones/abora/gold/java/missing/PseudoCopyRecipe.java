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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import info.dgjones.abora.gold.cxx.otherclass.CopyRecipe;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;


public class PseudoCopyRecipe extends CopyRecipe {

	public PseudoCopyRecipe(AboraClass cat, Association cuisine) {
		super(cat.getCategory(), cuisine);
		// TODO Auto-generated constructor stub
	}

	protected static Recipe associationToRecipe(Association assoc) {
		return (Recipe)assoc.value();
	}


	public PseudoCopyRecipe() {
		super();
		// TODO Auto-generated constructor stub
	}

	public PseudoCopyRecipe(Rcvr receiver) {
		super(receiver);
		// TODO Auto-generated constructor stub
	}

	//TODO override to simply get something to work here
	public Heaper parse(SpecialistRcvr rcvr) {
		try {
		Class receiverClass = categoryOfDish().brotherClass().getJavaClass();
		Constructor constructor = receiverClass.getConstructor(new Class[] {Rcvr.class});
		System.out.println("Parse: "+constructor);
//		rcvr.registerIbid(heaper); // TODO pushed into Heaper(Rcvr) - probably totally wrong!
		Heaper heaper = (Heaper) constructor.newInstance(new Object[] {rcvr});
		//TODO receive heaper.receive(rcvr);
		return heaper;
		} catch (InvocationTargetException e) {
			throw new AboraRuntimeException("Failed to call constructor for new object: "+rcvr, e);
		} catch (NoSuchMethodException e) {
			throw new AboraRuntimeException("Failed to find constructor for new object: "+rcvr, e);
		} catch (IllegalAccessException e) {
			throw new AboraRuntimeException("Failed to access new object: "+rcvr, e);
		} catch (InstantiationException e) {
			throw new AboraRuntimeException("Failed to instantiate: "+rcvr, e);
		}
	
//		Heaper result = ;
////		AboraSupport.translateOnly();
//		{
//			/*  
//		result = Heaper::operator new (0, xcsj, this->categoryOfDish()); */
//		}
////		AboraSupport.smalltalkOnly();
//		{
//			result = new DeletedHeaper();
//		}
//		rcvr.registerIbid((result));
//		parseInto(rcvr, (PtrArray) result);
//		return result;
//
	}

}
