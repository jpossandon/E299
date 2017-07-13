if strcmp(exp.sTtyp,'singleLH')
    if result.block_crossed(next_block)==0 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==0 && result.blockType(next_block)==2
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==2
        display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    end
elseif strcmp(exp.sTtyp,'handEye')
    if result.block_crossed(next_block)==0 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES OPEN**\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==0 && result.blockType(next_block)==2
        display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS UNCROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES CLOSED**\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==1
        display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES OPEN**\n',sum(result.block_done)+1))
    elseif result.block_crossed(next_block)==1 && result.blockType(next_block)==2
        display(sprintf('\n\nBLOCK # %d START,\nHAND POSITION IS CROSSED\n INSTRUCTION IS TO RESPOND WITH THE SAME LIMB SIDE\n**EYES CLOSED**\n',sum(result.block_done)+1))
    end
elseif strcmp(exp.sTtyp,'LH2cross')
    if result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    end
  elseif strcmp(exp.sTtyp,'LH2crossAnti')
    if result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CROSSED\nINSTRUCTION IS TO RESPOND TO THE OPPOSITE LIMB SIDE\n',sum(result.block_done)+1))
    end  
elseif strcmp(exp.sTtyp,'LH2crossHpos')
    if result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CENTERED AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CENTERED AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CENTERED AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS CENTERED AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CENTERED AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CENTERED AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CENTERED AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 2
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS CENTERED AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS TO THE RIGHT AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS TO THE RIGHT AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS TO THE RIGHT AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==0 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS UNCROSSED\nHAND POSITION IS TO THE RIGHT AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS TO THE RIGHT AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==0 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS TO THE RIGHT AND UNCROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==1 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS TO THE RIGHT AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME EXTERNAL SIDE\n',sum(result.block_done)+1))
    elseif result.block_crossed_legs(next_block)==1 && result.block_crossed_hands(next_block)==1 && result.blockType(next_block)==2 && result.block_handpos(next_block) == 3
         display(sprintf('\n\nBLOCK # %d START,\nLEG POSITION IS CROSSED\nHAND POSITION IS TO THE RIGHT AND CROSSED\nINSTRUCTION IS TO RESPOND TO THE SAME LIMB SIDE\n',sum(result.block_done)+1))
    
    end
end