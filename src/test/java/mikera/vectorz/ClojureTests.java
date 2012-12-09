package mikera.vectorz;

import java.util.Arrays;
import java.util.List;

import mikera.cljunit.ClojureTest;

public class ClojureTests extends ClojureTest {

	@Override
	public List<String> namespaces() {
		return Arrays.asList(new String[] {
			"mikera.vectorz.test-core",
			"mikera.vectorz.test-matrix"			
		});
		
	}
}
