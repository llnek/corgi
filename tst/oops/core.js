// Compiled by ClojureScript 1.10.238 {}
goog.provide('oops.core');
goog.require('cljs.core');
goog.require('cljs.spec.alpha');
goog.require('goog.object');
goog.require('oops.sdefs');
goog.require('oops.state');
goog.require('oops.config');
goog.require('oops.messages');
goog.require('oops.helpers');
goog.require('oops.schema');
oops.core.report_error_dynamically = (function oops$core$report_error_dynamically(msg,data){
if(oops.state.was_error_reported_QMARK_.call(null)){
return null;
} else {
oops.state.mark_error_reported_BANG_.call(null);

var G__2777 = oops.config.get_error_reporting.call(null);
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"throw","throw",-1044625833),G__2777)){
throw oops.state.prepare_error_from_call_site.call(null,msg,oops.helpers.wrap_data_in_enveloper_if_possible.call(null,oops.config.use_envelope_QMARK_.call(null),data));
} else {
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"console","console",1228072057),G__2777)){
return oops.state.get_console_reporter.call(null).call(null,(console["error"]),msg,oops.helpers.wrap_data_in_enveloper_if_possible.call(null,oops.config.use_envelope_QMARK_.call(null),data));
} else {
if(cljs.core._EQ_.call(null,false,G__2777)){
return null;
} else {
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2777)].join('')));

}
}
}
}
});
oops.core.report_warning_dynamically = (function oops$core$report_warning_dynamically(msg,data){
var G__2778 = oops.config.get_warning_reporting.call(null);
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"throw","throw",-1044625833),G__2778)){
throw oops.state.prepare_error_from_call_site.call(null,msg,oops.helpers.wrap_data_in_enveloper_if_possible.call(null,oops.config.use_envelope_QMARK_.call(null),data));
} else {
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"console","console",1228072057),G__2778)){
return oops.state.get_console_reporter.call(null).call(null,(console["warn"]),msg,oops.helpers.wrap_data_in_enveloper_if_possible.call(null,oops.config.use_envelope_QMARK_.call(null),data));
} else {
if(cljs.core._EQ_.call(null,false,G__2778)){
return null;
} else {
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2778)].join('')));

}
}
}
});
oops.core.report_if_needed_dynamically = (function oops$core$report_if_needed_dynamically(var_args){
var args__4502__auto__ = [];
var len__4499__auto___2786 = arguments.length;
var i__4500__auto___2787 = (0);
while(true){
if((i__4500__auto___2787 < len__4499__auto___2786)){
args__4502__auto__.push((arguments[i__4500__auto___2787]));

var G__2788 = (i__4500__auto___2787 + (1));
i__4500__auto___2787 = G__2788;
continue;
} else {
}
break;
}

var argseq__4503__auto__ = ((((1) < args__4502__auto__.length))?(new cljs.core.IndexedSeq(args__4502__auto__.slice((1)),(0),null)):null);
return oops.core.report_if_needed_dynamically.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4503__auto__);
});

oops.core.report_if_needed_dynamically.cljs$core$IFn$_invoke$arity$variadic = (function (msg_id,p__2781){
var vec__2782 = p__2781;
var info = cljs.core.nth.call(null,vec__2782,(0),null);

if(cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),msg_id)){
} else {
var G__2785_2789 = oops.config.get_config_key.call(null,msg_id);
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"warn","warn",-436710552),G__2785_2789)){
oops.core.report_warning_dynamically.call(null,oops.messages.runtime_message.call(null,msg_id,info),info);
} else {
if(cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"error","error",-978969032),G__2785_2789)){
oops.core.report_error_dynamically.call(null,oops.messages.runtime_message.call(null,msg_id,info),info);
} else {
if(cljs.core._EQ_.call(null,false,G__2785_2789)){
} else {
if(cljs.core._EQ_.call(null,null,G__2785_2789)){
} else {
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2785_2789)].join('')));

}
}
}
}
}

return null;
});

oops.core.report_if_needed_dynamically.cljs$lang$maxFixedArity = (1);

/** @this {Function} */
oops.core.report_if_needed_dynamically.cljs$lang$applyTo = (function (seq2779){
var G__2780 = cljs.core.first.call(null,seq2779);
var seq2779__$1 = cljs.core.next.call(null,seq2779);
var self__4486__auto__ = this;
return self__4486__auto__.cljs$core$IFn$_invoke$arity$variadic(G__2780,seq2779__$1);
});

oops.core.validate_object_access_dynamically = (function oops$core$validate_object_access_dynamically(obj,mode,key,push_QMARK_,check_key_read_QMARK_,check_key_write_QMARK_){
if(((((cljs.core._EQ_.call(null,mode,(0))) && ((void 0 === obj))))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"undefined",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):((((cljs.core._EQ_.call(null,mode,(0))) && ((obj == null))))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"nil",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):(cljs.core.truth_(goog.isBoolean(obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"boolean",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):(cljs.core.truth_(goog.isNumber(obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"number",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):(cljs.core.truth_(goog.isString(obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"string",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):((cljs.core.not.call(null,goog.isObject(obj)))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"non-object",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return false;
})()
):(cljs.core.truth_(goog.isDateLike(obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"date-like",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return true;
})()
):(cljs.core.truth_(oops.helpers.cljs_type_QMARK_.call(null,obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"cljs type",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return true;
})()
):(cljs.core.truth_(oops.helpers.cljs_instance_QMARK_.call(null,obj))?((cljs.core.contains_QMARK_.call(null,oops.config.get_suppress_reporting.call(null),new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301)))?true:(function (){
oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"unexpected-object-value","unexpected-object-value",-1214439301),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"flavor","flavor",-1331636636),"cljs instance",new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

return true;
})()
):true
)))))))))){
if(cljs.core.truth_(push_QMARK_)){
oops.state.add_key_to_current_path_BANG_.call(null,key);

oops.state.set_last_access_modifier_BANG_.call(null,mode);
} else {
}

var and__3911__auto__ = (cljs.core.truth_(check_key_read_QMARK_)?((((cljs.core._EQ_.call(null,mode,(0))) && (cljs.core.not.call(null,goog.object.containsKey(obj,key)))))?oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"missing-object-key","missing-object-key",-1300201731),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null)):true):true);
if(cljs.core.truth_(and__3911__auto__)){
if(cljs.core.truth_(check_key_write_QMARK_)){
var temp__5459__auto__ = oops.helpers.get_property_descriptor.call(null,obj,key);
if((temp__5459__auto__ == null)){
if(cljs.core.truth_(oops.helpers.is_object_frozen_QMARK_.call(null,obj))){
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"object-is-frozen","object-is-frozen",-1391578096),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));
} else {
if(cljs.core.truth_(oops.helpers.is_object_sealed_QMARK_.call(null,obj))){
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"object-is-sealed","object-is-sealed",-1791813926),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));
} else {
return true;

}
}
} else {
var descriptor_2790 = temp__5459__auto__;
var temp__5459__auto____$1 = oops.helpers.determine_property_non_writable_reason.call(null,descriptor_2790);
if((temp__5459__auto____$1 == null)){
return true;
} else {
var reason_2791 = temp__5459__auto____$1;
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"object-key-not-writable","object-key-not-writable",206336031),new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"frozen?","frozen?",613726824),oops.helpers.is_object_frozen_QMARK_.call(null,obj),new cljs.core.Keyword(null,"reason","reason",-2070751759),reason_2791,new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));
}
}
} else {
return true;
}
} else {
return and__3911__auto__;
}
} else {
return null;
}
});
oops.core.validate_fn_call_dynamically = (function oops$core$validate_fn_call_dynamically(fn,mode){
if(((cljs.core._EQ_.call(null,mode,(1))) && ((fn == null)))){
return true;
} else {
if(cljs.core.truth_(goog.isFunction(fn))){
return true;
} else {
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"expected-function-value","expected-function-value",-1399123630),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"path","path",-188191168),oops.state.get_key_path_str.call(null),new cljs.core.Keyword(null,"soft?","soft?",-1339668477),cljs.core._EQ_.call(null,mode,(1)),new cljs.core.Keyword(null,"fn","fn",-1175266204),fn,new cljs.core.Keyword(null,"obj","obj",981763962),oops.state.get_target_object.call(null)], null));

}
}
});
oops.core.punch_key_dynamically_BANG_ = (function oops$core$punch_key_dynamically_BANG_(obj,key){
var child_factory_2793 = oops.config.get_child_factory.call(null);
var child_factory_2793__$1 = (function (){var G__2794 = child_factory_2793;
var G__2794__$1 = (((G__2794 instanceof cljs.core.Keyword))?G__2794.fqn:null);
switch (G__2794__$1) {
case "js-obj":
return ((function (G__2794,G__2794__$1,child_factory_2793){
return (function (){
return {};
});
;})(G__2794,G__2794__$1,child_factory_2793))

break;
case "js-array":
return ((function (G__2794,G__2794__$1,child_factory_2793){
return (function (){
return [];
});
;})(G__2794,G__2794__$1,child_factory_2793))

break;
default:
return child_factory_2793;

}
})();

var child_obj_2792 = child_factory_2793__$1.call(null,obj,key);
if(oops.core.validate_object_access_dynamically.call(null,obj,(2),key,false,true,true)){
(obj[key] = child_obj_2792);
} else {
}

return child_obj_2792;
});
oops.core.build_path_dynamically = (function oops$core$build_path_dynamically(selector){
if(((typeof selector === 'string') || ((selector instanceof cljs.core.Keyword)))){
var selector_path_2798 = [];
oops.schema.prepare_simple_path_BANG_.call(null,selector,selector_path_2798);

return selector_path_2798;
} else {
var selector_path_2799 = [];
oops.schema.prepare_path_BANG_.call(null,selector,selector_path_2799);

return selector_path_2799;

}
});
oops.core.check_path_dynamically = (function oops$core$check_path_dynamically(path,op){
var temp__5461__auto__ = oops.schema.check_dynamic_path_BANG_.call(null,path,op);
if((temp__5461__auto__ == null)){
return null;
} else {
var issue_2800 = temp__5461__auto__;
return cljs.core.apply.call(null,oops.core.report_if_needed_dynamically,issue_2800);
}
});
oops.core.get_key_dynamically = (function oops$core$get_key_dynamically(obj,key,mode){
if(oops.core.validate_object_access_dynamically.call(null,obj,mode,key,true,true,false)){
return (obj[key]);
} else {
return null;
}
});
oops.core.set_key_dynamically = (function oops$core$set_key_dynamically(obj,key,val,mode){
if(oops.core.validate_object_access_dynamically.call(null,obj,mode,key,true,true,true)){
return (obj[key] = val);
} else {
return null;
}
});
oops.core.get_selector_dynamically = (function oops$core$get_selector_dynamically(obj,selector){
if(cljs.core.truth_(((cljs.core.not.call(null,cljs.spec.alpha.valid_QMARK_.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector)))?(function (){var explanation_2809 = cljs.spec.alpha.explain_data.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector);
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"invalid-selector","invalid-selector",1262807990),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"explanation","explanation",-1426612608),explanation_2809,new cljs.core.Keyword(null,"selector","selector",762528866),selector], null));
})():true))){
var path_2802 = (function (){var path_2801 = oops.core.build_path_dynamically.call(null,selector);
oops.core.check_path_dynamically.call(null,path_2801,(0));

return path_2801;
})();
var len_2803 = path_2802.length;
var i_2804 = (0);
var obj_2805 = obj;
while(true){
if((i_2804 < len_2803)){
var mode_2806 = (path_2802[i_2804]);
var key_2807 = (path_2802[(i_2804 + (1))]);
var next_obj_2808 = oops.core.get_key_dynamically.call(null,obj_2805,key_2807,mode_2806);
var G__2810 = mode_2806;
switch (G__2810) {
case (0):
var G__2812 = (i_2804 + (2));
var G__2813 = next_obj_2808;
i_2804 = G__2812;
obj_2805 = G__2813;
continue;

break;
case (1):
if(!((next_obj_2808 == null))){
var G__2814 = (i_2804 + (2));
var G__2815 = next_obj_2808;
i_2804 = G__2814;
obj_2805 = G__2815;
continue;
} else {
return null;
}

break;
case (2):
if(!((next_obj_2808 == null))){
var G__2816 = (i_2804 + (2));
var G__2817 = next_obj_2808;
i_2804 = G__2816;
obj_2805 = G__2817;
continue;
} else {
var G__2818 = (i_2804 + (2));
var G__2819 = oops.core.punch_key_dynamically_BANG_.call(null,obj_2805,key_2807);
i_2804 = G__2818;
obj_2805 = G__2819;
continue;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2810)].join('')));

}
} else {
return obj_2805;
}
break;
}
} else {
return null;
}
});
oops.core.get_selector_call_info_dynamically = (function oops$core$get_selector_call_info_dynamically(obj,selector){
if(cljs.core.truth_(((cljs.core.not.call(null,cljs.spec.alpha.valid_QMARK_.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector)))?(function (){var explanation_2845 = cljs.spec.alpha.explain_data.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector);
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"invalid-selector","invalid-selector",1262807990),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"explanation","explanation",-1426612608),explanation_2845,new cljs.core.Keyword(null,"selector","selector",762528866),selector], null));
})():true))){
var path_2821 = (function (){var path_2820 = oops.core.build_path_dynamically.call(null,selector);
oops.core.check_path_dynamically.call(null,path_2820,(0));

return path_2820;
})();
var len_2822 = path_2821.length;
if((len_2822 < (4))){
return [obj,(function (){var path_2824 = path_2821;
var len_2825 = path_2824.length;
var i_2826 = (0);
var obj_2827 = obj;
while(true){
if((i_2826 < len_2825)){
var mode_2828 = (path_2824[i_2826]);
var key_2829 = (path_2824[(i_2826 + (1))]);
var next_obj_2830 = oops.core.get_key_dynamically.call(null,obj_2827,key_2829,mode_2828);
var G__2846 = mode_2828;
switch (G__2846) {
case (0):
var G__2850 = (i_2826 + (2));
var G__2851 = next_obj_2830;
i_2826 = G__2850;
obj_2827 = G__2851;
continue;

break;
case (1):
if(!((next_obj_2830 == null))){
var G__2852 = (i_2826 + (2));
var G__2853 = next_obj_2830;
i_2826 = G__2852;
obj_2827 = G__2853;
continue;
} else {
return null;
}

break;
case (2):
if(!((next_obj_2830 == null))){
var G__2854 = (i_2826 + (2));
var G__2855 = next_obj_2830;
i_2826 = G__2854;
obj_2827 = G__2855;
continue;
} else {
var G__2856 = (i_2826 + (2));
var G__2857 = oops.core.punch_key_dynamically_BANG_.call(null,obj_2827,key_2829);
i_2826 = G__2856;
obj_2827 = G__2857;
continue;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2846)].join('')));

}
} else {
return obj_2827;
}
break;
}
})()];
} else {
var target_obj_2823 = (function (){var path_2831 = path_2821.slice((0),(len_2822 - (2)));
var len_2832 = path_2831.length;
var i_2833 = (0);
var obj_2834 = obj;
while(true){
if((i_2833 < len_2832)){
var mode_2835 = (path_2831[i_2833]);
var key_2836 = (path_2831[(i_2833 + (1))]);
var next_obj_2837 = oops.core.get_key_dynamically.call(null,obj_2834,key_2836,mode_2835);
var G__2847 = mode_2835;
switch (G__2847) {
case (0):
var G__2859 = (i_2833 + (2));
var G__2860 = next_obj_2837;
i_2833 = G__2859;
obj_2834 = G__2860;
continue;

break;
case (1):
if(!((next_obj_2837 == null))){
var G__2861 = (i_2833 + (2));
var G__2862 = next_obj_2837;
i_2833 = G__2861;
obj_2834 = G__2862;
continue;
} else {
return null;
}

break;
case (2):
if(!((next_obj_2837 == null))){
var G__2863 = (i_2833 + (2));
var G__2864 = next_obj_2837;
i_2833 = G__2863;
obj_2834 = G__2864;
continue;
} else {
var G__2865 = (i_2833 + (2));
var G__2866 = oops.core.punch_key_dynamically_BANG_.call(null,obj_2834,key_2836);
i_2833 = G__2865;
obj_2834 = G__2866;
continue;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2847)].join('')));

}
} else {
return obj_2834;
}
break;
}
})();
return [target_obj_2823,(function (){var path_2838 = [(path_2821[(len_2822 - (2))]),(path_2821[(len_2822 - (1))])];
var len_2839 = path_2838.length;
var i_2840 = (0);
var obj_2841 = target_obj_2823;
while(true){
if((i_2840 < len_2839)){
var mode_2842 = (path_2838[i_2840]);
var key_2843 = (path_2838[(i_2840 + (1))]);
var next_obj_2844 = oops.core.get_key_dynamically.call(null,obj_2841,key_2843,mode_2842);
var G__2848 = mode_2842;
switch (G__2848) {
case (0):
var G__2868 = (i_2840 + (2));
var G__2869 = next_obj_2844;
i_2840 = G__2868;
obj_2841 = G__2869;
continue;

break;
case (1):
if(!((next_obj_2844 == null))){
var G__2870 = (i_2840 + (2));
var G__2871 = next_obj_2844;
i_2840 = G__2870;
obj_2841 = G__2871;
continue;
} else {
return null;
}

break;
case (2):
if(!((next_obj_2844 == null))){
var G__2872 = (i_2840 + (2));
var G__2873 = next_obj_2844;
i_2840 = G__2872;
obj_2841 = G__2873;
continue;
} else {
var G__2874 = (i_2840 + (2));
var G__2875 = oops.core.punch_key_dynamically_BANG_.call(null,obj_2841,key_2843);
i_2840 = G__2874;
obj_2841 = G__2875;
continue;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2848)].join('')));

}
} else {
return obj_2841;
}
break;
}
})()];
}
} else {
return null;
}
});
oops.core.set_selector_dynamically = (function oops$core$set_selector_dynamically(obj,selector,val){
if(cljs.core.truth_(((cljs.core.not.call(null,cljs.spec.alpha.valid_QMARK_.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector)))?(function (){var explanation_2890 = cljs.spec.alpha.explain_data.call(null,new cljs.core.Keyword("oops.sdefs","obj-selector","oops.sdefs/obj-selector",655346305),selector);
return oops.core.report_if_needed_dynamically.call(null,new cljs.core.Keyword(null,"invalid-selector","invalid-selector",1262807990),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"explanation","explanation",-1426612608),explanation_2890,new cljs.core.Keyword(null,"selector","selector",762528866),selector], null));
})():true))){
var path_2877 = (function (){var path_2876 = oops.core.build_path_dynamically.call(null,selector);
oops.core.check_path_dynamically.call(null,path_2876,(1));

return path_2876;
})();
var len_2880 = path_2877.length;
var parent_obj_path_2881 = path_2877.slice((0),(len_2880 - (2)));
var key_2878 = (path_2877[(len_2880 - (1))]);
var mode_2879 = (path_2877[(len_2880 - (2))]);
var parent_obj_2882 = (function (){var path_2883 = parent_obj_path_2881;
var len_2884 = path_2883.length;
var i_2885 = (0);
var obj_2886 = obj;
while(true){
if((i_2885 < len_2884)){
var mode_2887 = (path_2883[i_2885]);
var key_2888 = (path_2883[(i_2885 + (1))]);
var next_obj_2889 = oops.core.get_key_dynamically.call(null,obj_2886,key_2888,mode_2887);
var G__2891 = mode_2887;
switch (G__2891) {
case (0):
var G__2893 = (i_2885 + (2));
var G__2894 = next_obj_2889;
i_2885 = G__2893;
obj_2886 = G__2894;
continue;

break;
case (1):
if(!((next_obj_2889 == null))){
var G__2895 = (i_2885 + (2));
var G__2896 = next_obj_2889;
i_2885 = G__2895;
obj_2886 = G__2896;
continue;
} else {
return null;
}

break;
case (2):
if(!((next_obj_2889 == null))){
var G__2897 = (i_2885 + (2));
var G__2898 = next_obj_2889;
i_2885 = G__2897;
obj_2886 = G__2898;
continue;
} else {
var G__2899 = (i_2885 + (2));
var G__2900 = oops.core.punch_key_dynamically_BANG_.call(null,obj_2886,key_2888);
i_2885 = G__2899;
obj_2886 = G__2900;
continue;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2891)].join('')));

}
} else {
return obj_2886;
}
break;
}
})();
return oops.core.set_key_dynamically.call(null,parent_obj_2882,key_2878,val,mode_2879);
} else {
return null;
}
});

//# sourceMappingURL=core.js.map
