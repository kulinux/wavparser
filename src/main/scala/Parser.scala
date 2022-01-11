package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer


object Parser {
    import ParserUtil._

    private val fmtHeaderProtocol: HeaderProtocolEntry[FmtHeader] = (
        HeaderProtocolEntryFixed[FmtHeader, String](4, toStr, (fmt, in) => fmt.copy(fmt = in.trim())) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](4, toInt, (fmt, in) => fmt.copy(fmtSize = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](2, toInt, (fmt, in) => fmt.copy(audioFormat = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](2, toInt, (fmt, in) => fmt.copy(numChannels = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](4, toInt, (fmt, in) => fmt.copy(sampleRate = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](4, toInt, (fmt, in) => fmt.copy(byteRate = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](2, toInt, (fmt, in) => fmt.copy(blockAlign = in)) andThen
        HeaderProtocolEntryFixed[FmtHeader, Int](2, toInt, (fmt, in) => fmt.copy(bitsPerSample = in))
    )

    private val dataHeaderProtocol: HeaderProtocolEntry[DataHeader] = new DataParser()

    private val waveHeaderProtocol: HeaderProtocolEntry[WavHeader] = (
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(size = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(wave = in)) andThen
        fmtHeaderProtocol.merge(FmtHeader.empty(), (wph, fmt) => wph.copy(fmt = fmt)) andThen
        new FmtExtraParamParser() andThen
        new DiscardUnknownChunks() andThen
        dataHeaderProtocol.merge(DataHeader.empty(), (wph, data) => wph.copy(data = data)) 
    )

    def wavHeaderParser(is: InputStream) : WavHeader = waveHeaderProtocol.readAndParse(WavHeader.empty(), is)

}


trait HeaderProtocolEntry[CLS] { self =>
    def readAndParse(wph: CLS, read: InputStream): CLS

    def andThen(other: HeaderProtocolEntry[CLS]): HeaderProtocolEntry[CLS] = new HeaderProtocolEntry[CLS] {
        def readAndParse(wph: CLS, read: InputStream): CLS = {
            val owph: CLS = self.readAndParse(wph, read)
            other.readAndParse(owph, read)
        }
     }

     def merge[CLS2](empty: CLS, compose: (CLS2, CLS) => CLS2): HeaderProtocolEntry[CLS2] = new HeaderProtocolEntry[CLS2] {
        def readAndParse(wph: CLS2, read: InputStream): CLS2 = {
            val res: CLS = self.readAndParse(empty, read)
            compose(wph, res)
        }
    }
}

case class HeaderProtocolEntryFixed[CLS, T](size: Int, parse: Array[Byte] => T, copy: (CLS, T) => CLS) extends HeaderProtocolEntry[CLS] {
    def readAndParse(wph: CLS, read: InputStream): CLS = copy(wph, parse(read.readNBytes(size)))
}

class FmtExtraParamParser extends HeaderProtocolEntry[WavHeader] {
    def readAndParse(wph: WavHeader, read: InputStream): WavHeader = {
        if(wph.fmt.audioFormat == WavHeader.PCM) return wph
        val size = ParserUtil.toInt(read.readNBytes(2))
        val data = read.readNBytes(size)
        wph.copy(fmt = wph.fmt.copy(extraParam = Some(FmtExtraParam(size, data))))
    }
}

class DiscardUnknownChunks extends HeaderProtocolEntry[WavHeader] {
    def readAndParse(wph: WavHeader, read: InputStream): WavHeader = {
        read.mark(100)
        val chunkId = ParserUtil.toStr(read.readNBytes(4))
        val chunkSize = ParserUtil.toInt(read.readNBytes(4))
        if(chunkId.equals("data")) {
            read.reset()
            return wph
        }
        val content = read.readNBytes(chunkSize)
        val res = wph.copy(unknownHeader = wph.unknownHeader :+ UnknownHeader(chunkId, content))
        readAndParse(res, read)
    }
}

class DataParser extends HeaderProtocolEntry[DataHeader] {
    def readAndParse(wph: DataHeader, read: InputStream): DataHeader = {
        read.mark(100)
        val chunkId = ParserUtil.toStr(read.readNBytes(4))
        val chunkSize = ParserUtil.toInt(read.readNBytes(4))
        if(chunkId.equals("data")) {
            return wph.copy(data = read.readNBytes(chunkSize))
        }
        read.reset()
        return wph
    }
}

object ParserUtil {
    def toStr(arr: Array[Byte]): String = return new String(arr)

    def toInt(arr: Array[Byte]): Int = {
        val bArr: Array[Byte] = Array(0x00, 0x00, 0x00, 0x00)
        Array.copy(arr, 0, bArr, 0, arr.size)
        ByteBuffer.wrap(bArr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
    }
}

