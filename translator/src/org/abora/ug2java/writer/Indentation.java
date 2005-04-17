package org.abora.ug2java.writer;

import java.io.PrintWriter;



public interface Indentation {
	public void increase();
	public void decrease();
	public void write(PrintWriter printWriter);
}
