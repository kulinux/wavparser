package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer

import cats._
import cats.data._
import cats.implicits._


object HeaderProtocolEntry {
    private def composeParserFunc[CLS, T, S](wav1: HeaderProtocolEntry[CLS, T], wav2: HeaderProtocolEntry[CLS, S], arr: Array[Byte]): (T, S) = {
        val arrSplit = arr.splitAt(wav1.bToRead)
        (wav1.parserFunc(arrSplit._1), wav2.parserFunc(arrSplit._2))
    }

    private def compLens[CLS, T, S](lens1: (CLS, T) => CLS, lens2: (CLS, S) => CLS, cls: CLS, ts: Tuple2[T, S]): CLS =
        lens2(lens1(cls, ts._1), ts._2)

    def compose[CLS, T, S](wav1: HeaderProtocolEntry[CLS, T], wav2: HeaderProtocolEntry[CLS, S]): HeaderProtocolEntry[CLS, (T, S)] =
        HeaderProtocolEntry[CLS, (T, S)](
                bToRead = wav1.bToRead + wav2.bToRead,
                composeParserFunc(wav1, wav2, _),
                (cls: CLS, tuple: Tuple2[T, S]) => compLens(wav1.lens, wav2.lens, cls, tuple))
}

case class HeaderProtocolEntry[CLS, T](bToRead: Int, parserFunc: Array[Byte] => T, lens: (CLS, T) => CLS) {
    def parse(in: CLS, read: Int => Array[Byte]) : CLS =
        lens(in, parserFunc(read(bToRead)))

    def andThen[S](other: HeaderProtocolEntry[CLS, S]): HeaderProtocolEntry[CLS, (T, S)] = HeaderProtocolEntry.compose(this, other)
}


object Parser {

    private val waveHeaderProtocol = (
        HeaderProtocolEntry[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](4, toInt, (wh, in) => wh.copy(size = in)) andThen
        HeaderProtocolEntry[WavHeader, String](4, toStr, (wh, in) => wh.copy(wave = in)) andThen
        HeaderProtocolEntry[WavHeader, String](4, toStr, (wh, in) => wh.copy(fmt = in.trim())) andThen
        HeaderProtocolEntry[WavHeader, Int](4, toInt, (wh, in) => wh.copy(fmtSize = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](2, toInt, (wh, in) => wh.copy(audioFormat = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](2, toInt, (wh, in) => wh.copy(numChannels = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](4, toInt, (wh, in) => wh.copy(sampleRate = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](4, toInt, (wh, in) => wh.copy(byteRate = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](2, toInt, (wh, in) => wh.copy(blockAlign = in)) andThen
        HeaderProtocolEntry[WavHeader, Int](2, toInt, (wh, in) => wh.copy(bitsPerSample = in)) 
    )

    def wavHeaderParser(is: InputStream) : WavHeader = {
        val res = WavHeader.empty()
        val res3 = waveHeaderProtocol.parse(res, is.readNBytes)
        res3
    }

    private def toStr(arr: Array[Byte]): String = return new String(arr)

    private def toHex(bytes: Array[Byte]): String = {
        val result = new StringBuilder();
        for (temp <- bytes) {
            result.append(String.format("%02x", temp));
        }
        result.toString();
    }


    private def toInt(arr: Array[Byte]): Int = {
        val bArr: Array[Byte] = Array(0x00, 0x00, 0x00, 0x00)
        Array.copy(arr, 0, bArr, 0, arr.size)
        //println(toHex(arr) + " -> " + toHex(bArr))
        ByteBuffer.wrap(bArr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
    }


}