# Eislerのインストール

## Windowsでのインストール

Eislerのインストールは単純です。
バイナリファイルをダウンロードし、パスの通ったディレクトリに置くだけです。

https://github.com/elkel53930/Eisler/releases/ でWindows用のバイナリを配布しています。

![GitHub](resource\Install\GitHub.png)

本ドキュメントは、「Beta release for C92」を前提に書かれています。

Downloadsから「ket.exe」をダウンロードし、パスの通ったディレクトリにおいてください。
コマンドラインシェルから以下のコマンドを実行してください。
```
> ket
```

以下のように表示されたらインストールは完了です。

```
Eisler translator to KiCad legacy netlist.
```

## Linuxでのインストール

詳細は省きますが、EislerはHaskellで書かれています。
ソースコードをダウンロードして、ビルドしてください。

```
> git clone https://github.com/elkel53930/Eisler
> cd src
> ghc Eisler.hs -o ket
> cp ket <パスの通ったディレクトリ>
```
