package javax.swing;

public class Foo {
	public JComponent component;
	
	public Foo() {
		Object o = component.accessibleContext; // accessibleContext is protected on JComponent
	}
}