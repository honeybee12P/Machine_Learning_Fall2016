#!/usr/bin/python
import operator
import os
import sys 
import math 


#####INTIALIZATIONS#####
num0 = 0
num1 = 1
wesley = 0
honor = 0
barclay = 0
list_class_test = []
list_all = []
list_all_entropy = []
list_information_gain = []
list_all_edited_attribute1 = []
list_all_edited_attribute0 = []
dataset = []
no_of_attributes = 0
subdataset0 = []
subdataset1 = []
list_tree = []
#cls0 = 0
#cls1 = 0
def majority(attributes, data, target):
    #find target attribute
    valFreq = {}
    #find target in data
    index = attributes.index(target)
    #calculate frequency of values in target attr
    for tuple in data:
        if (valFreq.has_key(tuple[index])):
            valFreq[tuple[index]] += 1 
        else:
            valFreq[tuple[index]] = 1
    max = 0
    major = ""
    for key in valFreq.keys():
        if valFreq[key]>max:
            max = valFreq[key]
            major = key
    return major
########FUNCTION FOR ENTROPY#########
def entropy(noofy,noofn):
	h =  -noofy * ((math.log( noofy )) / math.log( 2 )) + (-noofn * (math.log( noofn )) / math.log( 2 ))
	return h 
#######FUNCTION FOR finding the attribute with highest INFORMATION GAIN########

def information_gain(dataset,attributes):
        dataset[0] = attributes
	list_all_entropy = []

	no_of_attributes=len(attributes)
	print no_of_attributes
	#for each in dataset[:]:
		#print each
	cls0 = 0
	cls1 = 0
        for each in dataset:
	    	#print no_of_attributes-1
	    
  
		if  each[no_of_attributes-1] == '0':
			cls0 = cls0 +1 
                else:
        		cls1 = cls1 + 1
	 
	print cls0
	print cls1
	class_ratio = float(cls0)/cls1
	if  class_ratio < 0.1  or no_of_attributes==1:
	#if  class_ratio < 0.1 or class_ratio > 10 or no_of_attributes==1:
                print "reached leaf"
		return None
	for i in range(no_of_attributes-1):
		entropy_calculated_no = 0
        	entropy_calculated_yes = 0 
		attribute0 = 0
		attribute1 = 0
		class_no_of_0 = 0
        	class_no_of_1 = 0
		class_yes_of_0 = 0
		class_yes_of_1 = 0 
		for x in dataset[1:]:
                        if x[i] == '0':
                        	attribute0 = attribute0 + 1
				if x[no_of_attributes - 1]=='0':
					class_no_of_0=class_no_of_0 + 1
				else: 	
					class_no_of_1 = class_no_of_1 + 1
                        else:
				attribute1 = attribute1 + 1
                        	if x[no_of_attributes -1]=='0':
					class_yes_of_0 = class_yes_of_0 + 1
                        	else:    
                                	class_yes_of_1 = class_yes_of_1 + 1
		a = float(class_no_of_0) / float(attribute0)
	 	b = float(class_no_of_1) / attribute0
	 	c = float(class_yes_of_0) / attribute1
	 	d = float(class_yes_of_1) / attribute1
	 	entropy_calculated_no  = entropy(a , b)
		entropy_calculated_yes  = entropy(c , d)
        	e = float(attribute0)/(attribute0+attribute1)
		f = float(attribute1)/(attribute0+attribute1)
		list_all_entropy.append([dataset[0][i],(( e*entropy_calculated_no ) + ( f * entropy_calculated_yes))])
	class_0=0
	class_1=0
	for each in dataset[1:]:
		if each[no_of_attributes-1]=='0':
			class_0=class_0+1
		else:
			class_1=class_1+1
	g = class_0/float(class_0 + class_1) 
	h = class_1/float(class_0 + class_1)
	H_X=entropy(g,h)
	list_information_gain=[]
        for each in list_all_entropy:
		each[1]=H_X-each[1]
		list_information_gain.append(each)
	all_information_gain = sorted(list_information_gain, key=operator.itemgetter(1), reverse=True)
        print all_information_gain
	highest=all_information_gain[0][0]
	print "highest from IG\t", highest
        #if highest == None:
	#print "yay"
	#else:
	if highest == None:
		raise ValueError("Reached leaf")
	else:
		return highest
"""
######FUNCTION TO GET DATASET VOID OF ATTRIBUTE WITH HIGHEST INFORMATION GAIN########
def rest_of_table(index_of_attribute_with_highest_IG,dataset):
	list_all_edited_attribute0 = []
	list_all_edited_attribute1 = []
        list_all_edited_attribute0.append(dataset[0])
	list_all_edited_attribute1.append(dataset[0])
	tem_list0 = list_all_edited_attribute0[0]
	tem_list1 = list_all_edited_attribute1[0]
	for each in tem_list0:
                if str(each) == tem_list0[index_of_attribute_with_highest_IG]:
			list_all_edited_attribute0[0].remove(each)
	for each in dataset[1:]:
                if each[index_of_attribute_with_highest_IG] == '0':
                        del each[index_of_attribute_with_highest_IG]
                        list_all_edited_attribute0.append(each)
                elif each[index_of_attribute_with_highest_IG] == '1':
                        del each[index_of_attribute_with_highest_IG]
                        list_all_edited_attribute1.append(each)
	return list_all_edited_attribute0, list_all_edited_attribute1
"""
"""
##########FUNCTION FOR attribute with highest IG ############
def attribute_dataset(dataset0,dataset1=None) :
	print "the attributes0----------:",dataset0[0]
        highest0 = attribute_highest_IG(dataset0)
        if highest0!= None:
                index_of_attribute_with_highest_IG0 = dataset0[0].index(highest0)
                subdataset00, subdataset01 = rest_of_table(index_of_attribute_with_highest_IG0,dataset0)
                attribute_dataset(subdataset00,subdataset01)
        print "the node0",highest0
        if dataset1 != None:
                print "the attributes1:------",dataset1[0]
                highest1 = attribute_highest_IG(dataset1)
                index_of_attribute_with_highest_IG1 = dataset1[0].index(highest1)
                subdataset10, subdataset11 = rest_of_table(index_of_attribute_with_highest_IG1,dataset1)
                attribute_dataset(subdataset10,subdataset11)
                print "the node1",highest1
	
	#if None not in dataset[0]:
        
        #else:
        #if None not in dataset[0]:
        #print highest
        #print "Tree created"
        #endoftree()
        #else:
        #index_of_attribute_with_highest_IG = dataset[0].index(highest)
        #subdataset0, subdataset1 = rest_of_table(index_of_attribute_with_highest_IG,dataset)
        #attribute_dataset(subdataset1)
"""
#def information_gain():
"""	
def best_attribute(dataset, attributes, target):
	print "attributes from best_attribute\t",attributes
	maximum_information_gain = 0
	#for each in dataset:
		#print "dataset from best attribute function\t",each	
	
    	for each in attributes:
        	gain  = information_gain(attributes, dataset, target) 
        	if gain > maximum_information_gain:
            		maximum_information_gain = gain
            		best_attribute_to_split = each
    	
	for each in dataset:
		print each
	gain = information	_gain(attributes, dataset, target)
	best_attribute_split = gain
	return best_attribute_split

"""
def get_subdataset(dataset,attributes,best_attribute_to_split,values):
	target = "class"
	default = majority(attributes, dataset, target)


	if None not in attributes:	
		return default
	else:
		index_of_attribute_with_highest_IG = attributes.index(best_attribute_to_split)
	
        dataset_edited = []
	edited_attribute = attributes[:]
        edited_attribute.remove(best_attribute_to_split)
        #dataset_edited.append(attributes.remove(best_attribute_to_split))
        #list_all_edited_attribute1.append(attributes)
        #tem_list0 = list_all_edited_attribute0[0]
        #tem_list1 = list_all_edited_attribute1[0]
        #for each in tem_list0:
                #if str(each) == tem_list0[index_of_attribute_with_highest_IG]:
                        #list_all_edited_attribute0[0].remove(each)
        for each in dataset:
                if each[index_of_attribute_with_highest_IG] == values:
                        del each[index_of_attribute_with_highest_IG]
                        dataset_edited.append(each)
                """
		elif each[index_of_attribute_with_highest_IG] == val:
                        del each[index_of_attribute_with_highest_IG]
                        list_all_edited_attribute1.append(each)
        	"""
	return dataset_edited,edited_attribute
def createtree (dataset, attributes, noofrecursing):
	noofrecursing  = noofrecursing + 1
	#if attributes == 0:
		#return
	target = "class"
	#best_attribute_to_split = information_gain(dataset,attributes)    	
	default = majority(attributes, dataset, target)
	
	#if best_attribute_to_split == None:
		#print "reached l"
		#return
	 
	#print "best_attribute\t",best_attribute_to_split
	
	#new_tree = { best_attribute_to_split : {}}
	#print "new tree\t", new_tree
	values = ['0','1']
	if not dataset or (len(attributes) - 1) <= 0:
		
		return default
	else:
		best_attribute_to_split = information_gain(dataset,attributes)
		new_tree = { best_attribute_to_split : {}}
		for each in values:
			print each
			subdataset,edited_attribute = get_subdataset(dataset,attributes,best_attribute_to_split,each)
			#print subdataset
			edited_attri = edited_attribute
			#print attributes
			#edited_attribute.remove(best_attribute_to_split)
			print "edited attri", edited_attribute
			#print "hi"
			#if edited_attri == "None":
			#return default
			print "Hi"
			#else:
			subdecisiontree = createtree(subdataset, edited_attri, noofrecursing)
			#new_tree = { best_attribute_to_split : {}}
		
			new_tree[best_attribute_to_split][each] = subdecisiontree
			#print new_tree 
			#else:
			#continue
	return new_tree
######MAIN FUNCTION#######
def main():
	target = "class"
	with open(sys.argv[1]) as f:
    		lines = f.readlines()
    	for each in lines:
        	no_of_attributes = len(each.split())
         	list_all.append(each.split())
        attributes = list_all[0]
	list_all.remove(attributes)
	tree = createtree(list_all,attributes,0)
	print "tree\t", tree	
        
if __name__ == "__main__":
    main()

