class C {
    protected int c;
}

class B : C {
    protected var b = new B();
    b.c = 3; 
}

class A {
    public var a = new A();  
    a.b.c(); 
}

