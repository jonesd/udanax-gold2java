package org.abora.ug2java;

import java.io.PrintWriter;



public class SimpleIndenter implements Indentation {
	private int level = 0;
	
	public SimpleIndenter() {
		super();
	}

	public void increase() {
		++level;
	}

	public void decrease() {
		level = Math.max(level - 1, 0);
	}

	public void write(PrintWriter printWriter) {
		for (int i = 0; i < level; i++) {
			printWriter.print("\t");
		}
	}
}
