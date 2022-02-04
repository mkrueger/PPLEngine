using Xunit;
using PPLD.Decompiler;

namespace UnitTests;

public class ObfuscatorTests
{
    [Fact]
    public void SimpleDecode()
    {
        byte[] buffer = { 188, 113, 184, 117, 181, 219, 236, 219, 189, 187, 189 };
        Obfuscator.Decode(buffer);
        byte[] expected = { 25, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0 };
        Assert.Equal(expected, buffer);
    }
}
