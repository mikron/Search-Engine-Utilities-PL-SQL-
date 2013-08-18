create or replace package P_TEST86 is

   -- Constants
   ci_Arm constant int := 1;
   cv_TrimSep constant varchar2(3) := '~$~';

   function v_Upper(av_Source varchar2, ai_Lang int := ci_Arm) return varchar2;

   --******************************************************************************
   --***************** SOUNDEX ****************************************************
   --******************************************************************************

   function v_ArmSoundex(av_InStr varchar2, ai_lang int := 1) return varchar2;

   function f_CompareSoundex(av_Str1 varchar2, av_Str2 varchar2) return int;

   --******************************************************************************
   --***************** JARO WINKLER ***********************************************
   --******************************************************************************

   function f_CompareJaroWinkler(av_Str1 varchar2, av_Str2 varchar2,
                                 ab_LongerStrComp boolean := false) return float;

   --******************************************************************************
   --***************** LEVENSHTEIN ************************************************
   --******************************************************************************

   function f_CutoffDistance(av_Str1 varchar2, av_Str2 varchar2) return float;
   function f_RecursiveDistance(av_Str1 varchar2, av_Str2 varchar2) return float;
   function f_CompareLevenShtein(av_Str1 varchar2, av_Str2 varchar2) return float;

   --******************************************************************************
   --********************** DICE COMPARATOR ***************************************
   --******************************************************************************

   function f_DiceCompare(av_Str1 varchar2, av_Str2 varchar2) return float;

   --******************************************************************************
   --********************** JACARD INDEX COMPARE **********************************
   --******************************************************************************

   function f_JacardCompare(av_Str1 varchar2, av_Str2 varchar2) return float;

   --******************************************************************************
   --********************** OTHERS ************************************************
   --******************************************************************************
   function f_LevenSteinMetric(av_Str1 varchar2, av_Str2 varchar2,
                               ai_MaxEdits int) return float;
   function f_LevenSteinMetricPref(av_Str1 varchar2, av_Prefix varchar2,
                                   ai_Max int) return float;

end P_TEST86;
/
create or replace package body P_TEST86 is

   -- Link - http://hy.wikipedia.org/wiki/%D5%80%D5%B6%D5%B9%D5%B5%D5%B8%D6%82%D5%B6%D5%A1%D5%A2%D5%A1%D5%B6%D5%B8%D6%82%D5%A9%D5%B5%D5%B8%D6%82%D5%B6
   T_ArmVowels TT := TT('²', 'º', '¾', 'à', 'ô', 'Æ', 'ú', 'Ú');
   T_LikeConsonants TTT := TTT(TT('´', 'ä', 'ö'), TT('¶', 'Î', 'ø'),
                               TT('¸', 'î', 'Â'), TT('Ò', 'Ì', 'ò'),
                               TT('æ', 'Ö', 'â'), TT('¼', 'ê'), TT('Ä', 'Þ'),
                               TT('Ô', 'Ê', 'Ð'), TT('ì', 'ü'), TT('Ø', 'Ü'),
                               TT('ð', 'è'), TT('È'));

   function v_Upper(av_Source varchar2, ai_Lang int := ci_Arm) return varchar2 is
      lv_Upper varchar2(4000);
   begin
      if ai_Lang = ci_Arm then
         lv_Upper := translate(av_Source,
                               '³µ·¹»½¿ÁÃÅÇÉËÍÏÑÓÕ×ÙÛÝßáãåçéëíïñóõ÷ùûýabcdefghijklmnopqrstuvwxyz',
                               '²´¶¸º¼¾ÀÂÄÆÈÊÌÎÐÒÔÖØÚÜÞàâäæèêìîðòôöøúüABCDEFGHIJKLMNOPQRSTUVWXYZ');
      else
         lv_Upper := upper(av_Source);
      end if;
      return replace(lv_Upper, '¨', 'ºì');
   end;

   function L_Trim(av_Str varchar2, av_TrimSymb varchar2) return varchar2 is
      lv_ResStr varchar2(4000);
   begin
      if length(av_TrimSymb) = 1 then
         lv_ResStr := trim(trailing av_TrimSymb from av_Str);
      else
         lv_ResStr := replace(av_Str, av_TrimSymb, cv_TrimSep);
         lv_ResStr := rtrim(lv_ResStr, cv_TrimSep);
         lv_ResStr := replace(lv_ResStr, cv_TrimSep, av_TrimSymb);
      end if;
      return lv_ResStr;
   end;

   procedure swapT(T1 in out nocopy TT, T2 in out nocopy TT) is
      TMP TT;
   begin
      TMP := T1;
      T1 := T2;
      T2 := TMP;
   end;

   procedure swapT(T1 in out nocopy TTF, T2 in out nocopy TTF) is
      TMP TTF;
   begin
      TMP := T1;
      T1 := T2;
      T2 := TMP;
   end;

   /*
      **********************************************************************
      ************************* SOUNDEX ************************************
      **********************************************************************
      An implementation of the Soundex algorithm, and a comparator which
      considers strings to have a score of 0.9 if their Soundex values match.
   */

   function v_ArmSoundex(av_InStr varchar2, ai_lang int) return varchar2 is
      lv_InStr varchar2(4000) := v_Upper(av_InStr);
      lv_Result varchar2(4000);
      lv_Tmp varchar2(1);
   begin
      if length(lv_Instr) < 1 then
         return '';
      end if;
      if ai_lang <> 1 then
         return soundex(av_InStr);
      end if;
      -- Save first symbol
      lv_Result := substr(lv_InStr, 1, 1);
      -- Trim Armenian vowels
      lv_InStr := regexp_replace(lv_InStr, '²|º|¾|à|ô|Æ|ú|Ú');
      -- Get Armenian Consonants numbers
      for li in 1 .. Length(lv_InStr) loop
         lv_Tmp := substr(lv_InStr, li, 1);
         if not (ascii(lv_Tmp) between 178 and 252) then
            lv_Result := lv_Result || lv_Tmp;
         else
            for lj in 1 .. T_LikeConsonants.count loop
               for lk in 1 .. T_LikeConsonants(lj).count loop
                  --put(lv_Tmp, T_LikeConsonants(lj) (lk));
                  if lv_Tmp = T_LikeConsonants(lj) (lk) then
                     lv_Result := L_Trim(lv_Result, trim(to_char(lj, 'X'))) ||
                                  trim(to_char(lj, 'X'));
                  end if;
               end loop;
            end loop;
         end if;
      end loop;
   
      return lv_Result;
   exception
      when others then
         --put(av_InStr);
         return null;
   end;

   function f_CompareSoundex(av_Str1 varchar2, av_Str2 varchar2) return int is
   begin
      if av_Str1 = av_Str2 then
         return 1.0;
      elsif v_ArmSoundex(av_Str1) = v_ArmSoundex(av_Str2) then
         return 0.9;
      else
         return 0.0;
      end if;
   end;
   --**********************************************************************
   --****************** JARO WINKLER ALGORITHM ****************************
   --**********************************************************************

   function f_CompareJaroWinkler(av_Str1 varchar2, av_Str2 varchar2,
                                 ab_LongerStrComp boolean := false) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
      li_MaxDist int;
      li_CommonChars int := 0;
      li_TransposChars int := 0;
      li_PrevPos int := -1;
      lv_TempChar varchar2(1);
      li_Score float;
      li_PrefixLength int := 0;
      li_PrefixLengthMax int;
   begin
      if lv_Str1 = lv_Str2 then
         return 1;
      end if;
      /*if z_config.cb_Real then
         return p_string.f_BCompareJaroWinkler(lv_Str1, lv_Str2);
      end if;*/
      -- ensure that s1 is shorter than or same length as s2
      if length(lv_Str1) > length(lv_Str2) then
         swap(lv_Str1, lv_Str2);
      end if;
   
      -- (1) find the number of characters the two strings have in common.
      -- note that matching characters can only be half the length of the
      -- longer string apart.
      li_MaxDist := length(lv_Str2);
      for li in 1 .. length(lv_Str1) loop
         lv_TempChar := substr(lv_Str1, li, 1);
         for lj in Greatest(1, li - li_MaxDist) .. Least(length(lv_Str2),
                                                         li + li_MaxDist) loop
            if lv_TempChar = substr(lv_Str2, lj, 1) then
               li_CommonChars := li_CommonChars + 1;
               if li_PrevPos <> -1
                  and lj < li_PrevPos then
                  li_TransposChars := li_TransposChars + 1;
               end if;
               li_PrevPos := lj;
               exit;
            end if;
         end loop;
      end loop;
   
      -- if didn't found any Common characters exit
      if li_CommonChars = 0 then
         return 0;
      end if;
   
      -- first compute the score
      li_Score := (li_CommonChars / length(lv_Str1) +
                  li_CommonChars / length(lv_Str2) +
                  (li_CommonChars - li_TransposChars) / li_CommonChars) / 3;
   
      -- second common prefix modification
      li_PrefixLengthMax := Least(4, length(lv_Str1));
      while li_PrefixLength < li_PrefixLengthMax
            and substr(lv_Str1, li_PrefixLength, 1) =
            substr(lv_Str2, li_PrefixLength, 1) loop
         li_PrefixLength := li_PrefixLength + 1;
      end loop;
      li_Score := li_Score + (li_PrefixLength * (1 - li_Score)) / 10;
   
      -- longer string adjustment (if ab_LongerStrComp is true)
      if ab_LongerStrComp
         and length(lv_Str1) >= 5
         and /* both strings at least 5 characters long*/
         li_CommonChars - li_PrefixLength >= 2
         and /* at least two common characters besides prefix */
         li_CommonChars - li_PrefixLength >=
         (length(lv_Str1) - li_PrefixLength) / 2 /* fairly rich in common chars */
       then
         li_Score := li_Score +
                     (1 - li_Score) * (li_CommonChars - (li_PrefixLength + 1)) /
                     (length(lv_Str1) + length(lv_Str2) -
                     2 * (li_PrefixLength - 1));
      end if;
   
      return li_Score;
   end;

   --**********************************************************************
   --****************** LEVENSHTEIN ALGORITHM *****************************
   --**********************************************************************

   -- An optimized version of the Wagner & Fischer algorithm, which
   -- exploits our knowledge that if the distance is above a certain
   -- limit (0.5 when normalized) we use the lower probability. We
   -- therefore stop once we go over the maximum distance.
   -- <p>On at least one use case, this optimization shaves 15% off the
   -- total execution time (ie: not just Levenshtein).
   function f_CutoffDistance(av_Str1 varchar2, av_Str2 varchar2) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
   
      li_MaxDist int;
      li_Str1Len int := length(lv_Str1);
      lt_Matrix ttf := ttf();
      lv_TempChar varchar2(1);
      li_Cost int;
      li_Left int;
      li_Above int;
      li_AboveLeft int;
      li_Distance int;
   begin
      if length(lv_Str1) = 0 then
         return length(lv_Str2);
      elsif length(lv_Str2) = 0 then
         return length(lv_Str1);
      end if;
      li_MaxDist := Least(length(av_Str1), length(av_Str2)) / 2;
      -- we use a flat array for better performance. we address it by
      -- s1ix + s1len * s2ix. this modification improves performance
      -- by about 30%, which is definitely worth the extra complexity.      
      lt_Matrix.extend((li_Str1Len + 1) * (length(lv_Str2) + 1));
      for li in 1 .. (length(lv_Str2) + 1) loop
         lt_Matrix(li * li_Str1Len) := li;
      end loop;
      for lj in 1 .. (li_Str1Len + 1) loop
         lt_Matrix(lj) := lj;
      end loop;
      for li in 1 .. li_Str1Len loop
         lv_TempChar := substr(lv_Str1, li, 1);
         for lj in 1 .. length(lv_Str2) loop
            if lv_TempChar = substr(lv_Str2, lj, 1) then
               li_Cost := 0;
            else
               li_Cost := 1;
            end if;
            li_Left := lt_Matrix(li + (lj + 1) * li_Str1Len) + 1;
            li_Above := lt_Matrix(li + 1 + (lj * li_Str1Len)) + 1;
            li_AboveLeft := lt_Matrix(li + (lj * li_Str1Len)) + li_Cost;
            li_Distance := Least(li_Left, Least(li_Above, li_AboveLeft));
            if li = lj
               and li_Distance > li_MaxDist then
               return li_Distance;
            end if;
            lt_Matrix(li + 1 + (lj + 1) * li_Str1Len) := li_Distance;
         end loop;
      end loop;
      return lt_matrix(li_Str1Len + (Length(lv_Str2) * li_Str1Len));
   end;

   /**
   * This implementation is my own reinvention of Ukkonen's optimized
   * version of the Wagner & Fischer algorithm. It's not exactly the
   * same as Ukkonen's algorithm, and I only managed to formulate it
   * recursively. The result is that unless s1 and s2 are very similar
   * it is slower than Wagner & Fischer. I don't recommend using this
   * version.
   */

   function f_ComputeRecursively(at_Matrix in out nocopy ttf, av_Str1 varchar2,
                                 av_Str2 varchar2, ai_Str1Len int,
                                 ai_Str2Len int) return float is
      li_Pos int;
      li_Lowest int;
      li_Smallest int;
      li_CostSmallest int;
   
      li_Cost int;
      li_AboveLeft int;
      li_Above int;
      li_Left int;
      li_Distance int;
   begin
      -- for the first row and first column we know the score already
      if ai_Str1Len = 0 then
         return ai_Str1Len;
      elsif ai_Str2Len = 0 then
         return ai_Str2Len;
      end if;
      -- work out our position in the matrix, and see if we know the score
      li_pos := ai_Str1Len + (ai_Str2Len * length(av_Str1));
      if at_Matrix(li_Pos) <> -1 then
         return at_Matrix(li_pos);
      end if;
      -- the lowest possible score in this position
      li_Lowest := abs(ai_Str1Len - ai_Str2Len);
      -- increase estimate based on lowest score at diagonal
      li_Smallest := Least(ai_Str1Len, ai_Str2Len);
      li_CostSmallest := at_Matrix(li_Smallest + li_Smallest * length(av_Str1));
      if li_CostSmallest <> -1 then
         li_Lowest := li_CostSmallest + 1;
      end if;
      -- find the cost here      
      if substr(av_Str1, ai_Str1Len - 1, 1) =
         substr(av_Str2, ai_Str2Len - 1, 1) then
         li_Cost := 0;
      else
         li_Cost := 1;
      end if;
      -- if aboveleft is already at the lowest, we're done
      li_AboveLeft := f_ComputeRecursively(at_matrix, av_Str1, av_Str2,
                                           ai_Str1Len - 1, ai_Str2Len - 1);
      if li_AboveLeft = li_Lowest then
         at_Matrix(li_pos) := li_lowest + li_Cost;
         return li_Lowest + li_Cost;
      end if;
      -- what about above?
      li_Above := f_ComputeRecursively(at_Matrix, av_Str1, av_Str2, ai_Str1Len,
                                       ai_Str2Len - 1);
      if li_Above > li_Lowest then
         -- could be lower than above, so compute
         li_left := f_ComputeRecursively(at_Matrix, av_Str1, av_Str2,
                                         ai_Str1Len - 1, ai_Str2Len);
      else
         -- it' can't be smaller than above, so no need to compute
         li_Left := li_Above;
      end if;
   
      li_Distance := Least(li_Left, Least(li_Above, li_Aboveleft)) + li_Cost;
      at_Matrix(li_pos) := li_Distance;
      return li_Distance;
   end;

   function f_RecursiveDistance(av_Str1 varchar2, av_Str2 varchar2) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
   
      lt_Matrix ttf := ttf(-1);
   begin
      if length(lv_Str1) = 0 then
         return length(lv_Str1);
      elsif length(lv_Str2) = 0 then
         return length(av_Str2);
      end if;
      -- we use a flat array for better performance. we address it by
      -- s1ix + s1len * s2ix. this modification improves performance
      -- by about 30%, which is definitely worth the extra complexity.      
      lt_Matrix.extend((length(lv_Str1) + 1) * (length(lv_Str2) + 1), 1);
      return f_ComputeRecursively(lt_Matrix, lv_Str1, lv_Str2, length(lv_Str1),
                                  length(lv_Str2));
   end;

   /**
   * Optimized version of the Wagner & Fischer algorithm that only
   * keeps a single column in the matrix in memory at a time. It
   * implements the simple cutoff, but otherwise computes the entire
   * matrix. It is roughly twice as fast as the original function.
   */

   function compactDistance(av_Str1 varchar2, av_Str2 varchar2) return float is
      li_Str1Len int := Length(av_Str1);
      li_Str2Len int := Length(av_Str2);
      lf_MaxDist float := Least(li_Str1Len, li_Str2Len) / 2;
      lt_Column TTF := TTF(0);
      li_Cost int;
      li_Above int := 0;
      lv_TempChar varchar2(1);
      li_Smallest int;
      li_Value int;
   begin
      if li_Str1Len = 0 then
         return li_Str2Len;
      end if;
      if li_Str2Len = 0 then
         return li_Str1Len;
      end if;
   
      -- we allocate just one column instead of the entire matrix, in
      -- order to save space.  this also enables us to implement the
      -- algorithm somewhat faster.  the first cell is always the
      -- virtual first row.
      lt_Column.extend(li_Str1Len + 1, 1);
   
      -- first we need to fill in the initial column. we use a separate
      -- loop for this, because in this case our basis for comparison is
      -- not the previous column, but a virtual first column.
      lv_TempChar := substr(av_Str2, 1, 1);
      lt_column(1) := 1; -- virtual first row
   
      for li in 2 .. li_Str1Len loop
         li_Cost := i_bool(substr(av_Str1, li - 1, 1) = lv_TempChar);
         -- Lowest of three: above (lt_column(li - 1)), aboveleft: li - 1,
         -- left: li. Latter cannot possibly be lowest, so is
         -- ignored.
         lt_Column(li) := Least(lt_Column(li - 1), li - 1) + li_Cost;
      end loop;
   
      -- okay, now we have an initialized first column, and we can
      -- compute the rest of the matrix.   
      for lj in 2 .. li_Str2Len loop
         lv_TempChar := substr(av_Str2, lj, 1);
         li_Above := lj + 1; -- virtual first row
         li_Smallest := li_Str1Len * 2;
         for lk in 2 .. li_Str1Len loop
            li_Cost := i_bool(substr(av_Str1, lk - 1, 1) = lv_TempChar);
            -- above:     li_above
            -- aboveleft: lt_column(lk - 1)
            -- left:      lt_column(lk)
            li_Value := Least(Least(li_Above, lt_Column(lk - 1), lt_Column(lk))) +
                        li_Cost;
            lt_Column(lk - 1) := li_Above;
            li_Above := li_Value;
            li_Smallest := Least(li_Smallest, li_Value);
         end loop;
         lt_Column(li_Str1Len) := li_Above;
      
         -- check if we can stop because we'll be going over the max distance
         if li_Smallest > lf_MaxDist then
            return li_Smallest;
         end if;
      end loop;
      return li_Above;
   end;

   function f_CompareLevenShtein(av_Str1 varchar2, av_Str2 varchar2) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
   
      li_MinLength int := Least(length(lv_Str2), Length(lv_Str2));
      li_MaxLength int := Greatest(length(lv_Str2), Length(lv_Str2));
   
      li_Dist int;
   begin
      -- we know that if the outcome here is 0.5 or lower, then the
      -- property will return the lower probability. so the moment we
      -- learn that probability is 0.5 or lower we can return 0.0 and
      -- stop. this optimization makes a perceptible improvement in
      -- overall performance.
      if li_MinLength / li_MaxLength <= 0.5 then
         return 0.0;
      end if;
   
      /* if z_config.cb_Real then
         return p_string.f_BCompareLevenShtein(av_Str1, av_Str2);
      end if;*/
   
      -- if the strings are equal we can stop right here.
      if li_MinLength = li_MaxLength
         and lv_Str1 = lv_Str2 then
         return 1.0;
      end if;
   
      -- we couldn't shortcut, so now we go ahead and compute the full
      -- metric
      li_Dist := Least(compactDistance(lv_Str1, lv_Str2), li_MaxLength);
      return 1 - li_Dist / li_MinLength;
   end;

   --******************************************************************************
   --********************** DICE COMPARATOR ***************************************
   --******************************************************************************

   /* function T_SplitStr(av_Str varchar2) return TT is
      Tokens TT := TT(null);
      li_Start int := 1;
      li_TCount int := 1;
      li_indx int;
      prevws boolean := false;
      TMP TT := TT(null);
   begin
      Tokens.extend(length(av_Str) / 2 + 1, 1);
      for li in 1 .. length(av_Str) loop
         li_indx := li;
         if substr(av_Str, li, 1) = ' ' then
            if not prevws
               and li > 1 then
               Tokens.extend;
               Tokens(Tokens.count) := substr(av_Str, li_Start, li);
            end if;         
            prevws := true;
            li_Start := li + 1;
         else
            prevws := false;
         end if;
      end loop;
      if not prevws and li_Start <> li_indx then
         Tokens(li_TCount) := substr(av_Str, li_Start);
         --li_TCount := li_TCount + 1;
      end if;
      TMP.Extend(li_TCount - 1, 1);
      for lj in 1..li_TCount loop
          TMP(lj) := Tokens(lj);
      end loop;    
      return TMP;
   end;*/

   function T_SplitStr(av_Str varchar2) return TT is
      T TT := TT();
   begin
      for li in 1 .. Length(av_Str) loop
         T.extend;
         T(T.count) := substr(av_Str, li, 1);
      end loop;
      return T;
   end;

   function f_DiceCompare(av_Str1 varchar2, av_Str2 varchar2) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
      T1 TT := T_SplitStr(lv_Str1);
      T2 TT := T_SplitStr(lv_Str2);
      li_Sum int := 0;
      li_highest int;
   begin
      if lv_Str1 = lv_Str2 then
         return 1;
      end if;
      -- ensure that t1 is shorter than or same length as t2
      if T1.count > T2.count then
         SwapT(T1, T2);
      end if;
      for li in 1 .. T1.count loop
         put(T1(li));
      end loop;
      for li in 1 .. T2.count loop
         put(T2(li));
      end loop;
      -- find best matches for each token in t1
      for li in 1 .. T1.count loop
         li_highest := 0;
         for lj in 1 .. T2.count loop
            li_highest := Greatest(li_highest, i_bool(T1(li) = T2(lj)));
         end loop;
         li_sum := li_sum + li_highest;
      end loop;
      return li_Sum * 2 /(T1.count + T2.count);
   end;

   --******************************************************************************
   --********************** JACARD INDEX COMPARE **********************************
   --******************************************************************************

   function f_JacardCompare(av_Str1 varchar2, av_Str2 varchar2) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
      T1 TT := T_SplitStr(lv_Str1);
      T2 TT := T_SplitStr(lv_Str2);
      lf_intersection float := 0;
      lf_union float := T1.count + T2.count;
      lf_highest float;
   begin
      if lv_Str1 = lv_Str2 then
         return 1;
      end if;
   
      if T1.count > T2.count then
         swapT(T1, T2);
      end if;
   
      for li in 1 .. T1.count loop
         lf_highest := 0;
         for lj in 1 .. T2.count loop
            lf_highest := Greatest(lf_highest, i_bool(T1(li) = T2(lj)));
         end loop;
         lf_intersection := lf_intersection + lf_highest;
         lf_union := lf_union - lf_highest;
      end loop;
      return lf_intersection / lf_union;
   end;

   --******************************************************************************
   --********************** OTHERS ************************************************
   --******************************************************************************   

   function f_LevenSteinMetric(av_Str1 varchar2, av_Str2 varchar2,
                               ai_MaxEdits int) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Str2 varchar2(4000) := v_Upper(av_Str2);
      li_FirstLength int := Length(lv_Str1);
      li_SecondLength int := Length(lv_Str2);
      lv_Tmp varchar2(4000);
      li_Max int := ai_MaxEdits;
      previousRow TTF := TTF(0);
      currentRow TTF := TTF(0);
      tempRow TTF := TTF();
      lv_Chr char(1);
      li_From int;
      li_To int;
      li_Cost int := 0;
   begin
      if li_FirstLength = 0 then
         return li_SecondLength;
      elsif li_SecondLength = 0 then
         return li_FirstLength;
      end if;
   
      if li_FirstLength > li_SecondLength then
         lv_Tmp := lv_Str1;
         lv_Str1 := lv_Str2;
         lv_Str2 := lv_Tmp;
         li_FirstLength := li_SecondLength;
         li_SecondLength := length(lv_Str2);
      end if;
   
      if nvl(li_Max, -1) < 0 then
         li_max := li_SecondLength;
      end if;
   
      if li_SecondLength - li_FirstLength > li_Max then
         return li_Max + 1;
      end if;
   
      if li_FirstLength > currentRow.count then
         currentRow.extend(li_FirstLength + 1, 1);
         previousRow.extend(li_FirstLength + 1, 1);
      end if;
   
      for li in 1 .. li_FirstLength loop
         previousRow(li) := li;
      end loop;
   
      for li in 1 .. li_SecondLength loop
         lv_Chr := substr(lv_Str2, li, 1);
         currentRow(1) := li;
         -- 
         li_From := Greatest(li - li_Max - 1, 1);
         li_To := Least(li + li_max + 1, li_FirstLength);
         for lj in li_From .. li_To loop
            -- 
            li_Cost := case when substr(lv_Str1, lj, 1) = lv_Chr then 0 else 1 end;
            currentRow(lj) := Least(Least(currentRow(lj) + 1,
                                          previousRow(lj) + 1),
                                    previousRow(lj) + li_cost);
         end loop;
         tempRow := previousRow;
         previousRow := currentRow;
         currentRow := tempRow;
      end loop;
   
      return previousRow(li_FirstLength);
   end;

   function f_LevenSteinMetricPref(av_Str1 varchar2, av_Prefix varchar2,
                                   ai_Max int) return float is
      lv_Str1 varchar2(4000) := v_Upper(av_Str1);
      lv_Prefix varchar2(4000) := v_Upper(av_Prefix);
      li_PrefixLength int := length(lv_Prefix);
      previousRow TTF := TTF(0);
      currentRow TTF := TTF(0);
      tempRow TTF := TTF();
      li_Max int := ai_max;
      li_StringLength int;
      li_Distance int; -- Integer.MAX_VALUE;
      lv_Chr char(1);
      li_From int;
      li_To int;
      li_Cost int;
   begin
      if nvl(li_max, -1) < 0 then
         li_max := li_PrefixLength;
      end if;
   
      li_StringLength := Least(length(lv_Str1), li_PrefixLength + li_max);
   
      if li_PrefixLength = 0 then
         return 0;
      elsif li_StringLength = 0 then
         return li_PrefixLength;
      end if;
   
      if li_StringLength < li_PrefixLength - li_max then
         return li_max + 1;
      end if;
   
      if li_PrefixLength > currentRow.count then
         currentRow.extend(li_PrefixLength + 1, 1);
         previousRow.extend(li_PrefixLength + 1, 1);
      end if;
   
      for li in 1 .. li_PrefixLength + 1 loop
         previousRow(li) := li;
      end loop;
   
      for li in 1 .. li_StringLength loop
         lv_Chr := substr(lv_Str1, li, 1);
         currentRow(1) := li;
         -- 
         li_From := Least(li - li_max - 1, 1);
         li_To := Least(li + li_max + 1, li_prefixLength);
         put(li_From, li_To);
         for lj in li_From .. li_To loop
            -- 
            li_cost := case when substr(lv_prefix, lj, 1) = lv_chr then 0 else 1 end;
            currentRow(lj) := Least(Least(currentRow(lj) + 1,
                                          previousRow(lj) + 1),
                                    previousRow(lj) + li_cost);
         end loop;
      
         -- 
         if li >= li_PrefixLength - li_max
            and li <= li_PrefixLength + li_Max
            and currentRow(li_prefixLength) < li_distance then
            li_distance := currentRow(li_prefixLength);
         end if;
      
         tempRow := previousRow;
         previousRow := currentRow;
         currentRow := tempRow;
      
      end loop;
   
      return li_Distance;
   
   end;

end P_TEST86;
/
