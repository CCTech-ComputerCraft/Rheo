class term
{
    public void clear()
    {
        doLua("term.clear()");
    }

    public void write(String arg)
    {
        doLua(join("", "term.write('", this.arg, "')"));
    }
}
