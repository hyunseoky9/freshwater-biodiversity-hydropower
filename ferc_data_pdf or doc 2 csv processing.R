# make a comprehensive measure file
rm(list=ls())

projd = read.csv('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv')

docid = 2150
projid = 2150
projd_idx = which(projd$documentID==sprintf('P-%d',docid) & 
                    grepl(projid,projd$FERC_project_No._as_stated_in_the_document))
projd[projd_idx,]
ifile_path = sprintf('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/P-%d_P-%d.txt',docid,projid)

#parameters
{
  pgnums = paste('4-',as.character(21:27),sep='') # use this if the pg numbers are dashed
  pgnums = as.character(204:215)
  consistent_gaps = NA#c(4,5) #not NA if gaps in the cost columns are consistent on the specified columns
  
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
  for(line in lines[(header_idx+1):length(lines)]) 
  {
    # it's a subheading
    if(grepl('sss',line))
    {
      row = c(gsub('sss','',line),rep(NA,numcol-1),1)
      r = rbind(r,row)
      next
    }
    if(grepl('\\$',line) | grepl('//',line)) # cost columns
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
      
      if(!grepl('//',line))
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
    } else{ 
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
      }
      
      if(any(ee)) # entitiy-only line (most likely) or measure-and-entitiy line
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
      } else if(any(ee2)) # entity-and-entity line
      {
        row_entities = c(row_entities,entities[which(ee2)])
        for(entity in entities[ee2])
        {
          line = trimws(gsub(entity,' ',line))
        }
      } else # measure-only line
      {
        row_measure = paste(row_measure, line,sep=' ')
      }
    }
  }
  # put in header as the colnames. add an extra column for subheadings.
  print(r)
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

