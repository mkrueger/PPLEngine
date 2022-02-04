namespace PPLD.Decompiler;

struct VarDecl
{
    public VariableType VariableType;
    public byte Dim;
    public int[] Dims;
    public ulong Content;
    public ulong Content2;
    public string StringValue;
    public byte Flag, LFlag, FFlag;
    public int Number;
    public byte Args, TotalVar;
    public ushort Start;
    public int FirstVar;
    public int ReturnVar;
    public int Func;
}
