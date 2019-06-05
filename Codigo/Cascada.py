# -*- coding: utf-8 -*-
"""
Programa que detecta rostros con base en cascada de Haar, recorta los rostros de
las imagenes, y ajusta dichas caras recortadas en una imagen de 100x100 pixeles.
Aparte de guardar las imagenes ya recortadas.

"""
import os
import glob
import numpy as np
import cv2

images = [cv2.imread(file) for file in glob.glob("C:/Users/DELL/Desktop/Hombres/*.jpg")]

face_cascade = cv2.CascadeClassifier('C:/Users/DELL/opencv_workspace/opencv/data/haarcascades/haarcascade_frontalface_alt.xml')

images2=[]
for i in images:
    gray = cv2.cvtColor(i, cv2.COLOR_BGR2GRAY)
    faces = face_cascade.detectMultiScale(gray, 1.3, 5)
    for (x,y,w,h) in faces:
        #cv2.rectangle(i,(x,y),(x+w,y+h),(255,0,0),2)
        roi_gray = gray[y:y+h, x:x+w]
        roi_color = i[y:y+h, x:x+w]
    crop_img = i[y:y+h, x:x+w]
    crop_img = cv2.resize(crop_img,(100,100))
    images2.append(crop_img)
    #nombre="woman"+chr(flag)
    #cv2.imwrite("a",crop_img)
for x in range(0, len(images2)):
    name="man"+str(x)+".jpg"
    cv2.imwrite(name,images2[x])
cv2.waitKey(0)
cv2.destroyAllWindows()
#cv2.imshow('img',img2)
#cv2.waitKey(0)
#crop_img = img[y:y+h, x:x+w]
#cv2.imshow("cropped", crop_img)
#cv2.imwrite("cropped2.jpg",crop_img)
#cv2.waitKey(0)
#
#cv2.destroyAllWindows()

