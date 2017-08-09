## シンプルなサンプル

### まずは作ってみよう

コネクタから電源を受け取り、LEDを点灯させるだけの回路を作りましょう。
回路図で書くと、以下のようになります。

![First_simple_sample_schematic](resource\First_simple_sample\schematic.png)

### コーディングと翻訳

テキストエディタに以下のソースコードを打ち込み、「Sample1.eis」というファイル名で保存します。
このソースコードが何を意味するかは後ほど説明します。

```tu01.eis
defpart CN(1:VIN 2:GND){ref "CN";}
defpart R(1:1 2:2){ref "R";}
defpart LED(1:A 2:K){ref "LD";}

defmodule main()
{
  part cn "PSS-410153-02" as CN;
  part led "BR1111C" as LED;
  part r "330 5% 1608" as R;

  wire VIN, GND;

  cn.VIN - VIN;
  cn.GND - GND;

  VIN - A.led.K - 1.r.2 - GND;
}
```

次に、お好みのコマンドラインシェルを開き、上記ファイルを保存したディレクトリに移動します。
そして以下のコマンドを実行してください。

```
> ket Sample1.eis
```

この処理を翻訳といいます。

エラーがなければ何も表示されずに翻訳は完了し、以下の５つのファイルが作られます。

* ConInfo.md (各部品ごとの結線情報)
* README.md (翻訳結果のサマリ)
* Sample1.asc (PADS用のネットリスト)
* Sample1.csv (部品表)
* Sample1.net (KiCad用のネットリスト)

### KiCadでの作業

KiCad用のネットリストが作成されるので、このネットリストをもとにアートワークを行います。
今回は、KiCadのバージョンとして「BZR 4022」を使用します。
これはいわゆるKiCad本(トランジスタ技術SPECIAL No.127)に付属していたバージョンです。
古いバージョンですが、最新のKiCad4系ではKiCad以外で作成したネットリストをインポートできないようですので、やむを得ずこのバージョンとしています。
このバージョンのKiCadはいまでもKiCadの日本ユーザコミュニティ( http://kicad.jp/ )からダウンロードすることができます。

このチュートリアルでは、KiCadそのものの操作方法について詳細な説明はしません。

#### プロジェクトの新規作成

KiCadを起動し、メニューから「ファイル→新規→プロジェクトを新規作成」を選んで新しいプロジェクトを作成します。
プロジェクト名はSample1.proとしました。

#### CvPcbによるフットプリントの関連付け

通常、KiCadで基板を開発する場合は、まずEESchemaを使って回路図を描きます。
しかしEislerを使う場合は回路図は不要で、部品とフットプリントを関連付けする作業から開始することになります。

KiCadのメインウィンドウの左から2番目のアイコンをクリックし、CvPcbを起動します。

![KiCad_main_CvPcb](resource\First_simple_sample\KiCad_main_CvPcb.png)

![CvPcb](resource\First_simple_sample\CvPcb.png)

CvPcbが起動したら、メニューから「ファイル→開く」を選び、Sample1.netを選択します。
「コンポーネント ライブラリ エラー」が出ますが、問題ありませんのでそのままOKをクリックしてください。

![CvPcb_file_opened](resource\First_simple_sample\CvPcb_file_opened.png)

部品にフットプリントを割り当てていきます。

|リファレンス|フットプリント|
|-|-|
|CN1|PIN_ARRAY_2X1|
|LD1|LED-0603|
|R1|SM0603|

![CvPcb_assoc](resource\First_simple_sample\CvPcb_assoc.png)
