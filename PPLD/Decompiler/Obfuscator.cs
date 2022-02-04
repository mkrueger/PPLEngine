using System;
namespace PPLD.Decompiler
{
	public static class Obfuscator
    {
        public static void DeCode2(byte[] block1, byte[] block2, int size, int size2)
        {
            int i = 0, j = 0;

            while ((i < size) && (j < size2))
            {
                block2[j++] = block1[i++];
                block2[j++] = block1[i++];
                if (block2[j - 1] == 0)
                {
                    while (block1[i] > 1)
                    {
                        block2[j++] = 0;
                        block1[i]--;
                    }
                    i++;
                }
                else
                {
                    if (block1[i] == 0)
                    {
                        block2[j] = 0;
                        i++; j++;
                        while (block1[i] > 1)
                        {
                            block2[j++] = 0;
                            block1[i]--;
                        }
                        i++;
                    }
                }
            }
        }

        public static void Decode(byte[] block)
        {
            ushort xorValue = 0xdb24;
            ushort j = (ushort)(block.Length / 2);
            int i = 0;
            byte rotateCount = 0;
            while (j > 0)
            {
                ushort curWord = (ushort)(block[i + 1] << 8 | block[i]);
                byte posXor = (byte)j;
                rotateCount = (byte)(xorValue + posXor);
                var outx = RotateRight(curWord, rotateCount) ^ xorValue;
                block[i++] = (byte)(outx ^ posXor);
                block[i++] = (byte)(outx >> 8 ^ posXor);
                xorValue = curWord;
                j--;
            }
            if (block.Length % 2 == 1)
            {
                block[i] = RotateRight((byte)(block[i] ^ xorValue), rotateCount);
            }
        }

        static byte RotateRight(byte value, byte count)
        {
            count %= 8;
            value = (byte)(value >> count | value << (8 - count));
            return value;
        }

        static ushort RotateRight(ushort value, byte count)
        {
            count %= 16;
            value = (ushort)(value >> count | value << (16 - count));
            return value;
        }
    }
}

