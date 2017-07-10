// v. 1.0

function fixTAKunits(JQobj, NextCount) {
    // In this context, JQobj is a JQuery Object representing the "template" block
    // The NextCount counter is one-indexed (i.e. starting at one)
    // First iterate over every element and find every attribute that contains TAKunit\d and update it...
    JQobj.find("*").each(function() {
        var JQSingleObj = $(this)
        //alert(JQSingleObj.html())
        $.each(this.attributes, function(i, attrib){
            var name = attrib.name;
            var value = attrib.value;
            //alert(name + ' = ' + value);
            // Note the fucky JS regex matching syntax: /TAKunit\d+/
            // Note that we use a global match (the 'g' suffix) to match all occurrences
            JQSingleObj.attr(name, value.replace(/TAKunit\d+/g, 'TAKunit'+NextCount));
        });
    });
    // Next we sort out the name array numbering (for PHP)
    JQobj.find("*[name]").each(function(){
        // This gets the first array number from the "template" block... 
        if ($(this).attr('name').match(/\[(\d+)\]/)) {
            arrayNo = parseInt($(this).attr('name').match(/\[(\d+)\]/)[1])
            // We use it to determine how to define the current array  number with respect to NextCount (which is the number of <divs> +1)
            if (arrayNo==0){
                // The template array was zero-indexed
                newArrayNo = NextCount-1;
            } else {
                // The template was not zero indexed
                newArrayNo = NextCount;
            }
        $(this).attr('name', $(this).attr('name').replace(/\[\d+\]/, '['+newArrayNo+']'));
        }
    });
    return JQobj;
};

$(document).ready(function() {
    var TAKctrlTemplates=[]
    // First, on document ready, we need to get the contents of each container as a template
    // We save it with 'deep' cloning in the TAKctrlTemplates array
    $(".TAKctrl-add-remove-container").each(function(){
        var ParentContents = $(this).clone(true,true).contents();
        var ParentID = $(this).attr("id");
        TAKctrlTemplates[ParentID] = ParentContents;
    });
    
    // Delegate a click event to the 'add' button
    $(".TAKctrl-add-remove-container").delegate('.TAKctrl-add', 'click', function() {
        // Find the main enclosing div
        var TAKctrlParentID = $(this).closest('.TAKctrl-add-remove-container').attr("id");
        // This should be an id like TAKctrl-??-contain
        // alert(TAKctrlParentID)
        // Find out how many DIVs we already have
        var CurrCount = $("#"+TAKctrlParentID+" > div").length;
        var NextCount = CurrCount+1;
        // Get the HTML of the first (hard coded) block, stored as a template in jStorage
        var TAKctrlBlockOne = TAKctrlTemplates[TAKctrlParentID].clone();
        // NB: These all must have a TAKunit suffix...
        // var TAKctrlBlockOne = $("#"+TAKctrlParentID+"-TAKunit1").html();
        // Construct the new ID
        
        var newID = TAKctrlParentID+'-TAKunit'+NextCount;
        TAKctrlBlockOne = fixTAKunits(TAKctrlBlockOne, NextCount)
        TAKctrlBlockOne.attr("id", newID)
        TAKctrlBlockOne.find(".TAKctrl-remove").show()
        // The following was to hide the "add" button, but it has bugs...
        // To make this work we should probably move it into the containing div and insert the new content above it...
        // $(this).hide()
        // Append it to the main (container) block
        $("#"+TAKctrlParentID).append(TAKctrlBlockOne);
        $("#"+TAKctrlParentID).trigger('TAKctrladd', newID);
    });
    $(".TAKctrl-initially-hidden").hide();
});

$(document).ready(function() {
    $(".TAKctrl-add-remove-container").delegate('.TAKctrl-remove', 'click', function() {
        // Find the closest div, then go one higher... TODO: Increase robustness
        var TAKctrlParentID = $(this).closest('.TAKctrl-add-remove-container').attr("id");
        var TAKctrlImmediateID = $(this).closest('[id^="'+TAKctrlParentID+'-"]').attr("id");
        // Could also use JQuery's "Contains Prefix Selector"
        var CurrCount = $("#"+TAKctrlParentID+" > div").length;
        // If there are more than 1 item, it is OK to remove...
        if(CurrCount>1) {
            $("#"+TAKctrlImmediateID).remove()
        }
    });
});