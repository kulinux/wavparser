package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer

trait HeaderProtocolEntry[CLS] { self =>
    def readAndParse(wph: CLS, read: InputStream): CLS

    def andThen(other: HeaderProtocolEntry[CLS]): HeaderProtocolEntry[CLS] = new HeaderProtocolEntry[CLS] {
        def readAndParse(wph: CLS, read: InputStream): CLS = {
            val owph: CLS = self.readAndParse(wph, read)
            other.readAndParse(owph, read)
        }
     }
}

case class HeaderProtocolEntryFixed[CLS, T](size: Int, parse: Array[Byte] => T, copy: (CLS, T) => CLS) extends HeaderProtocolEntry[CLS] {
    def readAndParse(wph: CLS, read: InputStream): CLS = copy(wph, parse(read.readNBytes(size)))
}

class FmtExtraParamParser extends HeaderProtocolEntry[WavHeader] {
    def readAndParse(wph: WavHeader, read: InputStream): WavHeader = {
        if(wph.fmt.audioFormat == WavHeader.PCM) return wph
        val size = Parser.toInt(read.readNBytes(2))
        val data = read.readNBytes(size)
        wph.copy(fmt = wph.fmt.copy(extraParam = Some(FmtExtraParam(size, data))))
    }
}

class DiscardUnknownChunks[CLS] extends HeaderProtocolEntry[CLS] {
    def readAndParse(wph: CLS, read: InputStream): CLS = {
        read.mark(100)
        val chunkId = Parser.toStr(read.readNBytes(4))
        val chunkSize = Parser.toInt(read.readNBytes(4))
        if(chunkId.equals("data")) {
            read.reset()
            return wph
        }
        read.readNBytes(chunkSize)
        return wph
    }
}

class DataParser extends HeaderProtocolEntry[WavHeader] {
    def readAndParse(wph: WavHeader, read: InputStream): WavHeader = {
        read.mark(100)
        val chunkId = Parser.toStr(read.readNBytes(4))
        val chunkSize = Parser.toInt(read.readNBytes(4))
        if(chunkId.equals("data")) {
            return wph.copy(data = wph.data.copy(data = read.readNBytes(chunkSize)))
        }
        read.reset()
        return wph
    }
}


object Parser {

    private val waveHeaderProtocol = (
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(size = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(wave = in)) andThen
        HeaderProtocolEntryFixed[WavHeader, String](4, toStr, (wh, in) => wh.copy(fmt = wh.fmt.copy(fmt = in.trim()))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(fmtSize = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(audioFormat = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(numChannels = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(sampleRate = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](4, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(byteRate = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(blockAlign = in))) andThen
        HeaderProtocolEntryFixed[WavHeader, Int](2, toInt, (wh, in) => wh.copy(fmt = wh.fmt.copy(bitsPerSample = in))) andThen
        new FmtExtraParamParser() andThen
        new DiscardUnknownChunks() andThen
        new DataParser()
    )

    def toStr(arr: Array[Byte]): String = return new String(arr)

     def toInt(arr: Array[Byte]): Int = {
        val bArr: Array[Byte] = Array(0x00, 0x00, 0x00, 0x00)
        Array.copy(arr, 0, bArr, 0, arr.size)
        ByteBuffer.wrap(bArr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
    }

    def wavHeaderParser(is: InputStream) : WavHeader = {
        val res = WavHeader.empty()
        val res3 = waveHeaderProtocol.readAndParse(res, is)
        res3
    }

}