Public Class Form1
    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub DDMT_TextChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub uAT_TextChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub Panel1_Paint(sender As Object, e As PaintEventArgs)

    End Sub




    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim sbo_rf As Double
        Dim r_hsw As Double
        Dim m_hsw As Double
        Dim r_sborf As Double

        If Double.TryParse(sborf.Text, sbo_rf) = False Then
            MessageBox.Show("Can't convert the SBO RF value !")
        ElseIf Double.TryParse(rhsw.Text, r_hsw) = False Then
            MessageBox.Show("Can't convert the Reqd H.S.W Value !")
        ElseIf Double.TryParse(mhsw.Text, m_hsw) = False Then
            MessageBox.Show("Can't convert the Measured HSW value !")

        Else
            sbo_rf = Val(sborf.Text)
            r_hsw = Val(rhsw.Text)
            m_hsw = Val(mhsw.Text)

            r_sborf = sbo_rf * m_hsw / r_hsw

            rsborf.Text = r_sborf.ToString("0.00")


        End If
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Dim fileReader As String
        'fileReader = My.Computer.FileSystem.ReadAllText("reading.txt")
        'Dim strArray() As String = fileReader.Split("|") 'change the | by your separator
        Tx1.Checked = True
        If Tx1.Checked Then
            sborf.Text = My.Settings.Sborf1
            sborf1.Text = My.Settings.Sborf1
            csbmod.Text = My.Settings.Csbmod1
            csdm.Text = My.Settings.Csdm1
            csbrf.Text = My.Settings.Csbrf1
            ccsbw.Text = My.Settings.Ccsbw1
            curmonreading.Text = My.Settings.Curmonreading1
            Nsborf.Text = My.Settings.Nsborf
            rhsw.Text = My.Settings.Rhsw1
            TXSBORF.Text = My.Settings.Sborf1
        Else
            sborf.Text = My.Settings.Sborf2
            sborf1.Text = My.Settings.Sborf2
            csbmod.Text = My.Settings.Csbmod2
            csdm.Text = My.Settings.Csdm2
            csbrf.Text = My.Settings.Csbrf2
            ccsbw.Text = My.Settings.Ccsbw2
            curmonreading.Text = My.Settings.Curmonreading2
            Nsborf.Text = My.Settings.Nsborf
            rhsw.Text = My.Settings.Rhsw2
            TXSBORF.Text = My.Settings.Sborf2

        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim N_sborf As Double
        Dim mlimit As Double
        Dim current_width As Double
        Dim wide_alarm As Double
        Dim wide_ddm As Double
        Dim narr_alarm As Double
        Dim narr_ddm As Double
        Dim tx_sborf As Double
        If Double.TryParse(Nsborf.Text, N_sborf) = False Then
            MessageBox.Show("Can't convert the CURRENT SBO RF value !")
        ElseIf Double.TryParse(limit.Text, mlimit) = False Then
            MessageBox.Show("Can't convert the Reqd ALARM LIMIT Value !")
        ElseIf Double.TryParse(curmonreading.Text, current_width) = False Then
            'ElseIf Double.TryParse(current_mon_wid.Text, current_width) = False Then
            MessageBox.Show("Can't convert the Current Monitor Width value !")

        Else
            N_sborf = Val(Nsborf.Text)
            tx_sborf = Val(TXSBORF.Text)
            mlimit = Val(limit.Text)
            current_width = Val(curmonreading.Text)

            wide_alarm = N_sborf * tx_sborf / 100 * (1 - mlimit / 100)
            narr_alarm = N_sborf * tx_sborf / 100 * (1 + mlimit / 100)
            wide_ddm = current_width * (1 - mlimit / 100)
            narr_ddm = current_width * (1 + mlimit / 100)

            widealarm.Text = wide_alarm.ToString("0.000")
            narrowalarm.Text = narr_alarm.ToString("0.000")
            widemonitor.Text = wide_ddm.ToString("0.000")
            narrowmonitor.Text = narr_ddm.ToString("0.000")


        End If
    End Sub

    Private Sub Label19_Click(sender As Object, e As EventArgs) Handles Label19.Click

    End Sub

    Private Sub Label20_Click(sender As Object, e As EventArgs) Handles Label20.Click

    End Sub

    Private Sub Bmodsum_Click(sender As Object, e As EventArgs) Handles Bmodsum.Click
        Dim csb_mod As Double
        Dim c_sdm As Double
        Dim r_sdm As Double
        Dim r_csbmod As Double

        If Double.TryParse(csbmod.Text, csb_mod) = False Then
            MessageBox.Show("Can't process the CSB MOD % value !")
        ElseIf Double.TryParse(csdm.Text, c_sdm) = False Then
            MessageBox.Show("Can't process the Current SDM Value !")
        ElseIf Double.TryParse(rsdm.Text, r_sdm) = False Then
            MessageBox.Show("Can't process the Required SDM value !")

        Else
            csb_mod = Val(csbmod.Text)
            c_sdm = Val(csdm.Text)
            r_sdm = Val(rsdm.Text)

            r_csbmod = csb_mod * r_sdm / c_sdm

            rcsbmod.Text = r_csbmod.ToString("0.00")


        End If

    End Sub

    Private Sub Bcsbrf_Click(sender As Object, e As EventArgs) Handles Bcsbrf.Click
        Dim csb_rf As Double
        Dim c_csbw As Double
        Dim r_csbw As Double
        Dim r_csbrf As Double

        If Double.TryParse(csbrf.Text, csb_rf) = False Then
            MessageBox.Show("Can't process the CSB MOD % value !")
        ElseIf Double.TryParse(ccsbw.Text, c_csbw) = False Then
            MessageBox.Show("Can't process the Current SDM Value !")
        ElseIf Double.TryParse(rcsbw.Text, r_csbw) = False Then
            MessageBox.Show("Can't process the Required SDM value !")

        Else
            csb_rf = Val(csbrf.Text)
            c_csbw = Val(ccsbw.Text)
            r_csbw = Val(rcsbw.Text)

            r_csbrf = csb_rf * r_csbw / c_csbw

            rcsbrf.Text = r_csbrf.ToString("0.00")


        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim sborf_1 As Double
        Dim c_width_ddm As Double
        Dim r_width_ddm As Double
        Dim r_sborf1 As Double

        If Double.TryParse(sborf1.Text, sborf_1) = False Then
            MessageBox.Show("Can't process the SBO RF % value !")
        ElseIf Double.TryParse(cwidthddm.Text, c_width_ddm) = False Then
            MessageBox.Show("Can't process the Current WIDTH DDM Value !")
        ElseIf Double.TryParse(rwidthddm.Text, r_width_ddm) = False Then
            MessageBox.Show("Can't process the Required WIDTH DDM value !")

        Else
            sborf_1 = Val(sborf1.Text)
            c_width_ddm = Val(cwidthddm.Text)
            r_width_ddm = Val(rwidthddm.Text)

            r_sborf1 = sborf_1 * r_width_ddm / c_width_ddm

            rsborf1.Text = r_sborf1.ToString("0.00")


        End If
    End Sub

    Private Sub CONVERT_Click_1(sender As Object, e As EventArgs) Handles CONVERT.Click
        Dim ddm As Double
        Dim ddmp As Double
        Dim uA As Double
        Dim meter As Double
        Dim feet As Double

        If ddmR.Checked AndAlso Double.TryParse(DDMT.Text, ddmp) = False Then
            MessageBox.Show("Can't convert the DDM % value !")
        ElseIf ddmpR.Checked AndAlso Double.TryParse(DDMPT.Text, ddm) = False Then
            MessageBox.Show("Can't convert the DDM value !")
        ElseIf uAR.Checked AndAlso Double.TryParse(uAT.Text, uA) = False Then
            MessageBox.Show("Can't convert the uA value !")
        ElseIf meterR.Checked AndAlso Double.TryParse(meterT.Text, meter) = False Then
            MessageBox.Show("Can't convert the METER value !")
        ElseIf feetR.Checked AndAlso Double.TryParse(feetT.Text, feet) = False Then
            MessageBox.Show("Can't convert the feet value !")


        Else
            If ddmR.Checked Then
                ddm = Val(DDMT.Text)
                ddmp = ddm * 100
                feet = ddm * 350 / 0.155
                uA = ddm * 150 / 0.155
                meter = feet * 0.3048

                feetT.Text = feet.ToString("0.00")
                meterT.Text = meter.ToString("0.00")

                uAT.Text = uA.ToString("0.000")

                DDMPT.Text = ddmp.ToString("0.00")

            ElseIf ddmpR.Checked Then
                ddmp = Val(DDMPT.Text)
                ddm = ddmp / 100
                feet = ddm * 350 / 0.155
                uA = ddm * 150 / 0.155
                meter = feet * 0.3048

                feetT.Text = feet.ToString("0.00")
                meterT.Text = meter.ToString("0.00")

                DDMT.Text = ddm.ToString("0.0000")
                uAT.Text = uA.ToString("0.000")



            ElseIf uAR.Checked Then
                uA = Val(uAT.Text)

                ddm = uA * 0.155 / 150
                ddmp = uA * 0.155 / 150 * 100
                feet = uA * 350 / 150
                meter = feet * 0.3048

                feetT.Text = feet.ToString("0.00")
                meterT.Text = meter.ToString("0.00")

                DDMPT.Text = ddmp.ToString("0.00")
                DDMT.Text = ddm.ToString("0.0000")


            ElseIf meterR.Checked Then
                meter = Val(meterT.Text)
                feet = meter / 0.3048

                uA = feet * 150 / 350
                ddm = feet * 0.155 / 350
                ddmp = feet * 0.155 / 350 * 100
                DDMPT.Text = ddmp.ToString("0.00")
                DDMT.Text = ddm.ToString("0.0000")
                uAT.Text = uA.ToString("0.00")
                feetT.Text = feet.ToString("0.00")

            ElseIf feetR.Checked Then
                feet = Val(feetT.Text)
                meter = feet * 0.3048

                uA = feet * 150 / 350
                ddm = feet * 0.155 / 350
                ddmp = feet * 0.155 / 350 * 100
                DDMPT.Text = ddmp.ToString("0.00")
                DDMT.Text = ddm.ToString("0.0000")
                uAT.Text = uA.ToString("0.00")
                meterT.Text = meter.ToString("0.00")


            End If



        End If

    End Sub

    Private Sub Tx1_CheckedChanged(sender As Object, e As EventArgs) Handles Tx1.CheckedChanged
        If Tx1.Checked Then
            sborf.Text = My.Settings.Sborf1
            sborf1.Text = My.Settings.Sborf1
            csbmod.Text = My.Settings.Csbmod1
            csdm.Text = My.Settings.Csdm1
            csbrf.Text = My.Settings.Csbrf1
            ccsbw.Text = My.Settings.Ccsbw1
            curmonreading.Text = My.Settings.Curmonreading1
            Nsborf.Text = My.Settings.Nsborf
            rhsw.Text = My.Settings.Rhsw1
            TXSBORF.Text = My.Settings.Sborf1
        Else
            sborf.Text = My.Settings.Sborf2
            sborf1.Text = My.Settings.Sborf2
            csbmod.Text = My.Settings.Csbmod2
            csdm.Text = My.Settings.Csdm2
            csbrf.Text = My.Settings.Csbrf2
            ccsbw.Text = My.Settings.Ccsbw2
            curmonreading.Text = My.Settings.Curmonreading2
            Nsborf.Text = My.Settings.Nsborf
            rhsw.Text = My.Settings.Rhsw2
            TXSBORF.Text = My.Settings.Sborf2
        End If
    End Sub

    Private Sub save_Click(sender As Object, e As EventArgs) Handles save.Click
        If Tx1.Checked Then
            My.Settings.Sborf1 = sborf.Text

            My.Settings.Csbmod1 = csbmod.Text
            My.Settings.Csdm1 = csdm.Text
            My.Settings.Csbrf1 = csbrf.Text
            My.Settings.Ccsbw1 = ccsbw.Text
            My.Settings.Curmonreading1 = curmonreading.Text
            My.Settings.Nsborf = Nsborf.Text
            My.Settings.Rhsw1 = rhsw.Text
        Else
            My.Settings.Sborf2 = sborf.Text

            My.Settings.Csbmod2 = csbmod.Text
            My.Settings.Csdm2 = csdm.Text
            My.Settings.Csbrf2 = csbrf.Text
            My.Settings.Ccsbw2 = ccsbw.Text
            My.Settings.Curmonreading2 = curmonreading.Text
            My.Settings.Nsborf = Nsborf.Text
            My.Settings.Rhsw2 = rhsw.Text

        End If
    End Sub

    Private Sub Panel6_Paint(sender As Object, e As PaintEventArgs) Handles Panel6.Paint

    End Sub

    Private Sub rsdm_TextChanged(sender As Object, e As EventArgs) Handles rsdm.TextChanged

    End Sub

    Private Sub RichTextBox3_TextChanged(sender As Object, e As EventArgs) Handles RichTextBox3.TextChanged

    End Sub
End Class
