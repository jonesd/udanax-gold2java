package org.abora.ug2java;

import java.io.PrintWriter;



public interface Indentation {
	public void increase();
	public void decrease();
	public void write(PrintWriter printWriter);
}
