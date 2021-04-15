import os

image_dir = "output/images/"
images = os.listdir(image_dir)
imgs = [image_dir + img for img in images]
sorted_images = sorted(imgs, key=lambda t: os.stat(t).st_mtime)
md = "images.md"

with open(md, "w") as f:
    f.write("# Images")
    for img in sorted_images:
        f.write(f"\n\n## {img}")
        f.write(f"\n\n![{img}](./{img})")
    f.write("\n\nRetour au [README](./README.md)\n")
