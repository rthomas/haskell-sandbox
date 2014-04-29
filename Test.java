public class Test {
    private String foo;
    private String bar = "bar";
    private static final String SOME_STRING = "SomeString";
    public Test(String foo) {
        this.foo = foo;
    }

    public static void main(String[] args) {
        Test t = new Test("foobar");
        System.out.println(t);
    }
}
