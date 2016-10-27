import os,sys
import matplotlib.pyplot as plt
from PIL import Image, ImageDraw, ImageFont, ImageFilter

if __name__ == "__main__":

        if len(sys.argv) != 7:
                sys.exit("Usage: PNGannotator.py <Input-Image> <Output-Image> <posx> <posy> <doblur: 0|1> <Text> ")

        infile = sys.argv[1]
        outfile = sys.argv[2]
        posx = int(sys.argv[3])
        posy = int(sys.argv[4])
        doblur = int(sys.argv[5])
        text1 = sys.argv[6]

        image = Image.open(infile)
  
        if doblur == 1:
                image = image.filter(ImageFilter.GaussianBlur(radius=10))

        draw  = ImageDraw.Draw(image)
        # font  = ImageFont.truetype("arial.ttf", 20, encoding="unic")

        draw.text( (posx,posy), text1, fill='#ffffa0')

        image.save(outfile)

