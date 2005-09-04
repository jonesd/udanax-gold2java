package org.abora.ug2java.util;

import java.util.StringTokenizer;



public class NameSupport {

	private NameSupport() {
		super();
	}

	public static String idToString(String id) {
		String s = "";
		StringTokenizer stringTokenizer = new StringTokenizer(id, "_", false);
		while (stringTokenizer.hasMoreTokens()) {
			s += capatilize(stringTokenizer.nextToken().toLowerCase());
		}
		return s;
	}
	
	public static String capatilize(String s) {
		char first = Character.toUpperCase(s.charAt(0));
		return Character.toString(first)+s.substring(1);
	}

	public static String idToLowerString(String id) {
		String s = "";
		StringTokenizer stringTokenizer = new StringTokenizer(id, "_", false);
		while (stringTokenizer.hasMoreTokens()) {
			s += stringTokenizer.nextToken().toLowerCase();
		}
		return s;
	}
}
