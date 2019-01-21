import os
import nester
import pickle

os.getcwd()
os.chdir('E:\\Users\\Stardust\\Desktop\\HeadFirstPython\\Chapter 3')
os.getcwd()
man = []
other = []

try:
    data = open('sketch.txt')
    for each_line in data:
        try:
            (role, line_spoken) = each_line.split(':', 1)
            line_spoken = line_spoken.strip()
            if role == 'Man':
                man.append(line_spoken)
            elif role == 'Other Man':
                other.append(line_spoken)
        except ValueError:
            pass
    data.close()
except IOError:
    print('The data is missing!')

try:
    with open('man_data.txt' , "wb") as man_file, open('other_data.txt', "wb") as other_file:
        pickle.dump(man, man_file)
        pickle.dump(other, other_file)

except IOError as err:
    print('File Error!' + str(err))

except pickle.PickleError as perr:
    print('Pickling Error! ' + str(perr))
    


