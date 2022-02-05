using PPLD.Decompiler;
var dir = Path.GetDirectoryName(typeof(Decompiler).Assembly.Location);
dir = Path.GetFullPath(Path.Combine(dir, "../../../../.."));
dir = Path.Combine(dir, "test_data", "eraseline_inputstr.ppe");
Decompiler.Read(dir);

Console.WriteLine("-----");
Console.WriteLine(File.ReadAllText(Path.ChangeExtension(dir, ".ppl")));