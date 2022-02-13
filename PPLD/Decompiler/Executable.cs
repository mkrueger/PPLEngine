namespace PPLD.Decompiler;

class Executable
{
    const int HeaderSize = 48;
    const int LastPPLC = 330;
    const string preamble = "PCBoard Programming Language Executable";

    public Version Version { get; private set; }
    public VarDecl[] VariableDeclarations { get; private set; }
    public short[] SourceBuffer { get; private set; }
    public int MaxVar { get; private set; }
    public int CodeSize { get; private set; }

    public static Executable Read(string fileName)
    {
        Executable result = new Executable();

        using var fs = new BinaryReader(File.OpenRead(fileName));
        var header = fs.ReadBytes(HeaderSize);

        if (System.Text.Encoding.ASCII.GetString(header, 0, preamble.Length) != preamble)
        {
            throw new InvalidOperationException("Invalid PPE file");
        }

        var version = ((header[40] & 15) * 10 + (header[41] & 15)) * 100 + (header[43] & 15) * 10 + (header[44] & 15);

        if (version > LastPPLC)
        {
            throw new NotSupportedException($"{version / 100}.{version % 100} not supported. Only versions up to 3.30.");
        }
        Console.WriteLine($"{version / 100}.{version % 100} detected");

        result.Version = new Version(version / 100, version % 100);
        result.MaxVar = fs.ReadUInt16();
        result.VariableDeclarations = ReadVars(fs, result.Version, result.MaxVar);

        var codeSize = fs.ReadUInt16();
        var curpos = fs.BaseStream.Position;
        var RealSize = fs.BaseStream.Length - curpos;
        var srcBuf = fs.ReadBytes((int)RealSize);

        if (result.Version.Major >= 3)
        {
            Obfuscator.Decode(srcBuf);
            if (RealSize != codeSize)
            {
                var srcBuf2 = new byte[codeSize];
                Obfuscator.DeCode2(srcBuf, srcBuf2, (int)RealSize, codeSize);

                srcBuf = srcBuf2;
            }
        }

        result.SourceBuffer = new short[srcBuf.Length / 2];
        for (int i = 0; i < srcBuf.Length; i += 2)
        {
            result.SourceBuffer[i / 2] = BitConverter.ToInt16(srcBuf, i);
        }
        result.CodeSize = codeSize - 2; //forget the last END
        return result;
    }

    static VarDecl[] ReadVars(BinaryReader fs, Version version, int maxVar)
    {
        var varCount = maxVar;
        if (varCount == 0)
            return null;
        varCount++;
        var variable_declarations = new VarDecl[varCount];
        while (varCount > 1)
        {
            var buf = fs.ReadBytes(11);
            if (version.Major >= 3)
            {
                Obfuscator.Decode(buf);
            }
            varCount = buf[1] << 8 | buf[0];
            var varDim = buf[2];
            var varDims = new[]
            {
                buf[4] << 8 | buf[3],
                buf[6] << 8 | buf[5],
                buf[8] << 8 | buf[7]
            };
            var varType = (VariableType)buf[9];
            variable_declarations[varCount - 1] = new VarDecl
            {
                Number = 0,
                Func = 0,
                VariableType = varType,
                Dim = varDim,
                Dims = varDims,
                Content = 0,
                Content2 = 0,
                StringValue = null,
                Flag = 0,
                LFlag = 0,
                FFlag = 0,
            };

            switch (varType)
            {
                case VariableType.String:
                    var varBuf = fs.ReadUInt16();
                    var stringBuffer = fs.ReadBytes(varBuf);
                    if (version.Major >= 3)
                    {
                        Obfuscator.Decode(stringBuffer);
                    }
                    variable_declarations[varCount - 1].StringValue = System.Text.Encoding.ASCII.GetString(stringBuffer);
                    break;
                case VariableType.Function:
                    buf = fs.ReadBytes(12);
                    if (version.Major >= 3)
                    {
                        Obfuscator.Decode(buf);
                    }

                    variable_declarations[varCount - 1].Args = buf[4];
                    variable_declarations[varCount - 1].TotalVar = (byte)(buf[5] - 1);
                    variable_declarations[varCount - 1].Start = (ushort)(buf[7] * 256 + buf[6]);
                    variable_declarations[varCount - 1].FirstVar = buf[9] * 256 + buf[8];
                    variable_declarations[varCount - 1].ReturnVar = buf[11] * 256 + buf[10];
                    break;
                case VariableType.Method:
                    buf = fs.ReadBytes(12);
                    if (version.Major >= 3)
                    {
                        Obfuscator.Decode(buf);
                    }
                    variable_declarations[varCount - 1].Args = buf[4];
                    variable_declarations[varCount - 1].TotalVar = buf[5];
                    variable_declarations[varCount - 1].Start = (ushort)(buf[7] * 256 + buf[6]);
                    variable_declarations[varCount - 1].FirstVar = buf[9] * 256 + buf[8];
                    variable_declarations[varCount - 1].ReturnVar = buf[11] * 256 + buf[10];
                    break;
                default:
                    if (version.Major == 1)
                    {
                        buf = fs.ReadBytes(4);
                        variable_declarations[varCount - 1].Content = fs.ReadUInt32();
                    }
                    else if (version.Major < 3)
                    {
                        buf = fs.ReadBytes(4);
                        variable_declarations[varCount - 1].Content = fs.ReadUInt64();
                    }
                    else
                    {
                        buf = fs.ReadBytes(12);
                        Obfuscator.Decode(buf);

                        for (int i = 0; i < 4; i++)
                        {
                            variable_declarations[varCount - 1].Content <<= 8;
                            variable_declarations[varCount - 1].Content |= buf[4 + i];
                        }
                        for (int i = 0; i < 4; i++)
                        {
                            variable_declarations[varCount - 1].Content2 <<= 8;
                            variable_declarations[varCount - 1].Content2 |= buf[8 + i];
                        }
                    }
                    break;
            }
        }
        int k = variable_declarations.Length - 1;
        while (k >= 0)
        {
            switch (variable_declarations[k].VariableType)
            {
                case VariableType.Function:
                    {
                        variable_declarations[variable_declarations[k].ReturnVar - 1].FFlag = 1;
                        var j = 0;
                        for (var i = variable_declarations[k].FirstVar; i < variable_declarations[k].TotalVar + variable_declarations[k].ReturnVar; i++)
                        {
                            variable_declarations[i].LFlag = 1;
                            if (j < variable_declarations[k].Args) variable_declarations[i].Flag = 1;
                            if (i != variable_declarations[k].ReturnVar - 1) variable_declarations[i].Number = (++j);
                        }
                    }
                    break;
                case VariableType.Method:
                    {
                        var j = 0;
                        for (var i = variable_declarations[k].FirstVar; i < variable_declarations[k].TotalVar + variable_declarations[k].Args + variable_declarations[k].FirstVar; i++)
                        {
                            variable_declarations[i].LFlag = 1;
                            if (j < variable_declarations[k].Args) variable_declarations[i].Flag = 1;
                            variable_declarations[i].Number = (++j);
                        }
                    }
                    break;
                default: break;
            }
            k--;
        }
        return variable_declarations;
    }
}
