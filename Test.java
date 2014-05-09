import java.io.Serializable;

public class Test implements Serializable {
    private String foo;
    private String bar = "bar";
    private static final String SOME_STRING = "SomeString";
    public Test(String foo) throws RuntimeException {
        this.foo = foo;
    }

    public static void main(String[] args) throws Exception {
        try {
            Test t = new Test("foobar");
            System.out.println(t);
        }
        catch (RuntimeException e) {
            System.out.println("catch");
        }
        finally {
            System.out.println("finally");
        }
    }
}
