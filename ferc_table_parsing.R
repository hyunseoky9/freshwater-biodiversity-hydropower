library(stringr)
rm(list=ls())

projd = read.csv('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv')

docid = 14276
projid = 14276

projd_idx = which(projd$documentID==sprintf('P-%d',docid) & 
                    grepl(projid,projd$FERC_project_No._as_stated_in_the_document))
projd[projd_idx,]
ifile_path = sprintf('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/P-%d_P-%s.txt',docid,projd$FERC_project_No._as_stated_in_the_document[projd_idx])

#parameters
{
  dollarsign_incost = 1
  no_cost_related_cost_col = 0 # if the cost data has no dollar signs, you need to specify this variable. (e.g. this is 1 is 'yes/no' column is in the cost columns)

  pgnums = sprintf('- %d -',86:90)
  
  pgnums = paste('4-',as.character(5:12),sep='') # use this if the pg numbers are dashed
  pgnums = as.character(114:122)
  
  consistent_gaps = c(NA) #not NA if gaps in the cost columns are consistent on the specified columns
  
  
  numcol = projd$numcol[projd_idx]
  numcostcol = projd$numcostcol[projd_idx] # number of columns related to costs + some other stuff (e.g., staff)
  entitycol = projd$entitycol[projd_idx] # there is an entity column
  
  if(entitycol)
  {
    entities = trimws(unlist(strsplit(projd$entities[projd_idx],',')))  
  } else {
    entities = NA
  }
}
pgnums
consistent_gaps
numcol
numcostcol
entitycol
entities = unique(sort(entities))
entities
lines <- readLines(ifile_path)

# get rid of page lines
if(length(unlist(apply(as.matrix(pgnums),1,function(x) which(lines==x))))>0)
{
  lines = lines[-unlist(apply(as.matrix(pgnums),1,function(x) which(lines==x)))]  
}
# get rid of document accession line
if(sum(unlist(apply(as.matrix(lines),1,function(x) grepl('Document Accession #',x))))>0)
{
  lines = lines[-which(unlist(apply(as.matrix(lines),1,function(x) grepl('Document Accession #',x))))]  
}

header_idx = which(grepl('hh',lines))[1]
header_idx
r = c()
row_entities = c()
row_measure = ''
row_cost = c()
{
  for(line in lines[(header_idx+1):length(lines)])#(header_idx+ii)]) 
  {
    print(line)
    # it's a subheading
    if(grepl('sss',line))
    {
      row = c(gsub('sss','',line),rep(NA,numcol-1),1)
      r = rbind(r,row)
      next
    }
    line_test = gsub('[a-zA-Z]*','',line)
    line_test = gsub('\\)','',line_test)
    line_test = gsub('\\(','',line_test)
    line_test = gsub(',','',line_test)
    nodolsign_test = (length(as.numeric(unlist(strsplit(str_squish(line_test),' ')))==3)+no_cost_related_cost_col)==numcostcol
    if((grepl('\\$',line) | grepl('//',line))|(dollarsign_incost==0 & nodolsign_test)) # cost columns
    {
      
      if(entitycol)
      {
        ecost = apply(as.matrix(entities),1,function(x) grepl(x,line)) # entity in cost columns      
      } else
      {
        ecost = FALSE
      }
      while(any(ecost))
      {
        row_entities = c(row_entities,entities[which(ecost)])
        # get rid of the entity in the cost column 
        line = trimws(gsub(paste(sample(entities[ecost],sum(ecost)),collapse=', '),'',line))
        ecost = apply(as.matrix(entities),1,function(x) grepl(x,line)) # entity in cost columns
      }
      
      
      if(!grepl('//',line) & dollarsign_incost)
      {
        # get rid of measure info from the cost column if there is one
        costcolsplit = unlist(strsplit(line,' '))
        # if 'gap' is positive, there's a gap in the cost columns.
        gap = numcostcol - max(which(grepl('\\$',rev(costcolsplit))))
        if((length(costcolsplit)+gap)>numcostcol)
        {
          measureINcost = paste(costcolsplit[1:(which(grepl('\\$',costcolsplit))[1]-1)],collapse=' ')
          row_measure = trimws(paste(row_measure, measureINcost,sep=' '))
          measureINcost_escaped <- gsub("([(])", "\\\\(", measureINcost)
          measureINcost_escaped <- gsub("([)])", "\\\\)", measureINcost_escaped)
          line = trimws(gsub(measureINcost_escaped,'',line))
        }
      } else {
        linesplit = unlist(strsplit(line,'///'))
        line = trimws(linesplit[length(linesplit)])
        if(length(linesplit)>1)
        {
          row_measure = trimws(paste(row_measure, linesplit[1],sep=' '))        
        }
      }
      # erase spaces in between commas if there is one in the cost columns
      line = gsub(', ',',',line)
      # split cost info into rows
      if(grepl('//',line))
      {
        row_cost = unlist(strsplit(line,'//'))
      } else
      {
        row_cost = unlist(strsplit(line,' '))
      }
      # if there's gaps in the cost column and it's predictable which column,
      # give NA to those column gaps
      if(all(!is.na(consistent_gaps)) & length(row_cost)<numcostcol)
      {
        row_cost_temp = rep(NA,numcostcol)
        row_cost_temp[-(consistent_gaps-(numcol-numcostcol))] = row_cost
        row_cost = row_cost_temp
      }
      if(length(which(row_cost=='nn')))
      {
        row_cost[which(row_cost=='nn')] = NA
      }
      #if(any(grepl('[A-Za-z]',row_cost) & nchar(row_cost)==1))
      #{
      #  row_cost = row_cost[-which(grepl('[A-Za-z]',row_cost))]
      #}
      if(length(row_cost)!=numcostcol)
      {
        print(row_cost)
        print(line)
        stop("row_cost length does not equal number of cost related columns")
      }
      if(entitycol)
      {
        row_entities = paste(unique(row_entities),collapse=', ')      
        r = rbind(r,c(row_measure,row_entities,row_cost,'0'))
      } else {
        r = rbind(r,c(row_measure,row_cost,'0'))
      }
      row_measure = ''
      row_entities = c()
      row_cost = c()
    } else
    { 
      #ee = apply(as.matrix(entities),1,function(x) grepl(x,line)) # entity in the line
      line_escaped = gsub(',','',line)
      line_escaped <- gsub("([(])", "\\\\(", line_escaped)
      line_escaped <- gsub("([)])", "\\\\)", line_escaped)
      if(entitycol)
      {
        ee = grepl(line_escaped,entities)  
        ee2 = apply(as.matrix(entities),1,function(x) grepl(x,line_escaped))
      } else
      {
        ee = FALSE
        ee2 = FALSE
      }
      
      if(any(ee)) # entitiy-only line or measure-and-entitiy line
      {
        row_entities = c(row_entities,entities[which(ee)])
        # find if the line is entity-and-measure line
        for(entity in entities[ee])
        {
          entity_words = unlist(strsplit(entity,' '))
          for(word in entity_words)
          {
            line = gsub(word,'',line)
          }
        }
        if(!trimws(gsub(',',' ',line))=='') # if entity-and-measure line
        {
          row_measure = paste(row_measure, line,sep=' ')
        }
      } else if(any(ee2)){
        row_entities = c(row_entities,entities[which(ee2)])
        for(entity in entities[ee2])
        {
          line = str_squish(trimws(gsub(entity,' ',line)))
        }
        if(!trimws(gsub(',',' ',line))=='') # if entity-and-measure line
        {
          row_measure = paste(row_measure, line,sep=' ')
        }
      } else # measure-only line
      {
        row_measure = paste(row_measure, line,sep=' ')
      }
    }
  }
  # put in header as the colnames. add an extra column for subheadings.
  
  colnames(r)=c(lines[1:(header_idx-1)],'subheader')
  # trim white space for all columns
  r = trimws(r)
  
  # get rid of superscripts in cost data values
  for(i in 1:ncol(r))
  {
    if(any(grepl('\\$',r[,i])))
    {
      spscript_idx = which(apply(as.matrix(r[,i]),1,function(x) if(!is.na(x)){sum(grepl("[a-zA-Z]",unlist(strsplit(x, ""))))}else{x=0})==1)
      if(length(spscript_idx>0))
      {
        r[spscript_idx,i] = gsub('[a-zA-Z]','',r[spscript_idx,i])
      }
    }
  }
  r = trimws(r) # trim again
  
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/P-%d_P-%d.csv',docid,projid)
  write.csv(r,ofilename,row.names=FALSE)
}
  



# get all the files from convert2excel processed files.
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/processed'
files = list.files(wd)
files = files[which(grepl('\\.csv',files))]
# change measure column name into 'measure' and get rid of returns
for(filename in files)
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  for(k in 1:ncol(d))
  {
    for(j in 1:nrow(d))
    {
      if(grepl('\n',d[j,k]))
      {
        d[j,1] = gsub('\n',' ',d[j,k])
      }
    }
  }
  names(d)[1] = 'measure'
  ofilename = ifilename
  write.csv(d,ofilename,row.names=FALSE)
  print(sprintf('%s done',filename))
}
# change annualized column name into 'annualized cost'
for(filename in files[164:length(files)])
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  print(head(d[,2:ncol(d)]))
  print(sprintf('%d.%s',1:ncol(d),names(d)))
  print('WHICH COLUMN IS THE ANNUALIZED COST?')
  var = readline()
  names(d)[as.integer(var)] = 'annualized cost'
  print(names(d))
  print('go ahead?')
  var2 = readline()
  if(var2 == 'y')
  {
    ofilename = ifilename
    write.csv(d,ofilename,row.names=FALSE)
    print(sprintf('%s done',filename))
  } else {
    next 
  }
}

# turn the annualized cost info into numerics.
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/processed'
files = list.files(wd)
files = files[which(grepl('\\.csv',files))]
for(filename in files[172:length(files)])
{
  ifilename = sprintf('%s/%s',wd,filename)
  print(filename)
  d = read.csv(ifilename)
  before = d$annualized.cost
  if(any(grepl('[a-zA-Z]',d$annualized.cost)))
  {
    d$annualized.cost = gsub('[a-zA-Z]','',d$annualized.cost)
    d$annualized.cost = gsub('\\$','',d$annualized.cost)
    d$annualized.cost = gsub('-$','',d$annualized.cost)
    d$annualized.cost = gsub('--$','',d$annualized.cost)
    d$annualized.cost = gsub('/','',d$annualized.cost)
    d$annualized.cost = trimws(gsub(',','',d$annualized.cost))
    if(any(na.omit(d$annualized.cost=="")))
    {
      d$annualized.cost[which(d$annualized.cost=="")] = NA
    }
    after = d$annualized.cost
    print(cbind(before,after))
    print('good to go?')
    val = readline()
    if(val=='y')
    {
      d$annualized.cost = as.numeric(d$annualized.cost)
      if(!all(na.omit(d$annualized.cost-floor(d$annualized.cost)==0)))
      {
        print('not all the cost values are integers? there might be an error in the cost values')
      }
      ofilename = ifilename
      write.csv(d,ofilename,row.names=FALSE)
      print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    } else {
      print(sprintf('stopped at %s (%d/%d)',filename,which(files==filename),length(files)))
      break
    }
  } else {
    print('no alphabets in the cost')
    d$annualized.cost = gsub('\\$','',d$annualized.cost)
    d$annualized.cost = gsub('-$','',d$annualized.cost)
    d$annualized.cost = gsub('--$','',d$annualized.cost)
    d$annualized.cost = gsub('/','',d$annualized.cost)
    d$annualized.cost = trimws(gsub(',','',d$annualized.cost))
    if(any(na.omit(d$annualized.cost=="")))
    {
      d$annualized.cost[which(d$annualized.cost=="")] = NA
    }
    after = d$annualized.cost
    print(cbind(before,after))
    print('good to go?')
    val = readline()
    if(val=='y')
    {
      d$annualized.cost = as.numeric(d$annualized.cost)
      if(!all(na.omit(d$annualized.cost-floor(d$annualized.cost)==0)))
      {
        print('not all the cost values are integers? there might be an error in the cost values')
      }
      ofilename = ifilename
      write.csv(d,ofilename,row.names=FALSE)
      print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    } else {
      print(sprintf('stopped at %s (%d/%d)',filename,which(files==filename),length(files)))
      break
    }
  }
}

# see which ones don't have subheaders and make a subheader column for those without
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/processed'
files = list.files(wd)
files = files[which(grepl('\\.csv',files))]
count = 0 # 81 with subheaders
for(filename in files[160:length(files)])
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  if(!'subheader' %in% names(d))
  {
    dtemp = subset(d,select=-c(measure,annualized.cost))
    d$subheader = 0
    #print(dtemp)
    subheading_idx = apply(dtemp,1,function(x) all(x==''))
    print(any(subheading_idx))
    if(any(subheading_idx))
    {
      print(d$measure[which(subheading_idx)])
      print(filename)
      d$subheader[which(subheading_idx)] = 1
      if(length(which(subheading_idx)[which(d$measure[which(subheading_idx)]=="")])>0)
      {
        d = d[-which(subheading_idx)[which(d$measure[which(subheading_idx)]=="")],] # get rid of NA rows        
      }
      print(dim(d))
    }
    readline()
    ofilename = ifilename
    write.csv(d,ofilename,row.names=FALSE)
    print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    #d$subheader = 0
    #print(sprintf('%s (%d/%d)',filename,which(files==filename),length(files)))
    #rid = readline()
    #if(rid!="")
    #{
    #  rids = as.numeric(unlist(strsplit(rid,',')))
    #  #rids = 
    #  d = d[-which(d$subheader==1)[rids],]
    #  ofilename = ifilename
    #  write.csv(d,ofilename,row.names=FALSE)
    #  print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    #}
  }
}
# get rid of 'X' columns
for(filename in files[1:length(files)])
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  if('X' %in% names(d))
  {
    #print(filename)
    print(sprintf('%s (%d/%d)',filename,which(files==filename),length(files)))
    print(sprintf('%d. %s',1:length(names(d)),names(d)))
    rid = readline()
    if(rid!="")
    {
      rids = as.numeric(unlist(strsplit(rid,',')))
      d = d[,-rids]
      print(names(d))
      ofilename = ifilename
      write.csv(d,ofilename,row.names=FALSE)
      print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    }
  }
}

# make adopted_by_staff column with 1 as adopted by staff and 0 as not.
for(filename in files[173:length(files)])
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  
  adopted = grepl('[aA]dopted',names(d))
  if(any(adopted))
  {
    names(d)[which(adopted)] = 'adopted.by.staff'
    newadopted = rep(0,nrow(d))
    print(unique(d$adopted.by.staff))
    print('everything ok?')
    val3 = readline()
    if(val3!='y')
    {
      print('SOMETHING WRONG.. TERMINATING.')
      break
    }
    # yes/no to 1/0
    newadopted[which(grepl('[yY]es',d$adopted.by.staff))] = 1
    newadopted[which(is.na(d$adopted.by.staff))] = NA
    d$adopted.by.staff = newadopted
    # write
    ofilename = ifilename
    write.csv(d,ofilename,row.names=FALSE)
    print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
  } else { # no adopted
    print(sprintf('%d. %s',1:length(names(d)),names(d)))
    print('is adopted by staff really not in the columns?')
    val = readline()
    if(val == 'y') # check for entity
    {
      # check for entity line
      entity = grepl('[eE]ntit',names(d))
      if(sum(entity)>1)
      {
        'more than two entity... check manually. Terming'
        print(sprintf('%s (%d/%d)',filename,which(files==filename),length(files)))
        break
      }
      if(any(entity))
      {
        names(d)[which(entity)] = 'entity'
        # check if staff is in it.
        newadopted = rep(0,nrow(d))
        newadopted[which(grepl('[sS]taff',d$entity))] = 1
        newadopted[which(grepl('FERC',d$entity))] = 1
        newadopted[which(grepl('FERC',d$entity))] = 1
        newadopted[which(is.na(d$entity)| d$entity=='')] = NA
        d$adopted.by.staff = newadopted
        if(sum(na.omit(newadopted))==0)
        {
          print('no staff recommendation at all, check manually. Terminating')
          print(sprintf('%s (%d/%d)',filename,which(files==filename),length(files)))
          break
        }
        #write
        ofilename = ifilename
        write.csv(d,ofilename,row.names=FALSE)
        print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
      } else { # entity not in any column names
        print(sprintf('%d. %s',1:length(names(d)),names(d)))
        print('is entity really not in the columns?')
        val = readline()
        if(val == 'y')
        {
          print('no info on staff adoption, check it manually. Terminating')
          print(sprintf('%s (%d/%d)',filename,which(files==filename),length(files)))
          break
        } else {
          print('which column is "entity"?')
          val2 = readline()
          names(d)[as.numeric(val2)] = 'entity'
          print(unique(d$entity))
          print('everything ok?')
          val3 = readline()
          if(val3!='y')
          {
            print('SOMETHING WRONG.. TERMINATING.')
            break
          }
          # check for staff
          newadopted = rep(0,nrow(d))
          newadopted[which(grepl('[sS]taff',d$entity))] = 1
          newadopted[which(is.na(d$entity)| d$entity=='')] = NA
          d$adopted.by.staff = newadopted
          # write
          ofilename = ifilename
          write.csv(d,ofilename,row.names=FALSE)
          print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
        }
      }
    } else { # adopted.by.staff in the data but the name was just odd.
      print('which column is "adopted by staff"?')
      val2 = readline()
      names(d)[as.numeric(val2)] = 'adopted.by.staff'
      newadopted = rep(0,nrow(d))
      print(unique(d$adopted.by.staff))
      print('everything ok?')
      val3 = readline()
      if(val3!='y')
      {
        print('SOMETHING WRONG.. TERMINATING.')
        break
      }
      #na yes & no
      newadopted[which(grepl('[yY]es',d$adopted.by.staff))] = 1
      newadopted[which(is.na(d$adopted.by.staff))] = NA
      d$adopted.by.staff = newadopted
      # write
      ofilename = ifilename
      write.csv(d,ofilename,row.names=FALSE)
      print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
    }
  }
}

# get rid of \n's in measure column again
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/processed'
files = list.files(wd)
files = files[which(grepl('\\.csv',files))]
# change measure column name into 'measure' and get rid of returns
for(filename in files)
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  for(j in 1:nrow(d))
  {
    if(grepl('\n',d$measure[j]))
    {
      d$measure[j] = gsub('\n',' ',d$measure[j])
    }
  }
  names(d)[1] = 'measure'
  ofilename = ifilename
  write.csv(d,ofilename,row.names=FALSE)
  print(sprintf('%s done',filename))
}



